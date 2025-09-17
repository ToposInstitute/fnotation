use std::collections::HashMap;

use crate::parser::*;
use crate::token::{self, *};
use crate::types::*;
use bumpalo::{collections::Vec as BumpVec, Bump};
use tattle::Reporter;

macro_rules! error {
    ($p:expr, $m:expr, $msg:literal) => {{
        $p.error($m, None, format!($msg))
    }};
    ($p:expr, $m:expr, $msg:literal, $($arg:expr),+) => {{
        $p.error($m, None, format!($msg, $($arg),+))
    }};
}

const ARG_START: &'static [token::Kind] = &[
    VAR, KEYWORD, OP, KEYWORD_OP, INT, FLOAT, LPAREN, LBRACK, LCURLY, STRING, PRIM, SPECIAL, TAG,
    FIELD,
];

type P<'a> = Parser<'a>;

fn parens<'a, F: FnMut(&P<'a>) -> PResult<'a>>(p: &P<'a>, mut f: F) -> PResult<'a> {
    let m = p.open_with(LPAREN);
    let s = f(p);
    p.eat_free(m, RPAREN)?;
    Ok(s?)
}

fn tuple<'a>(p: &P<'a>) -> Result<&'a FNtn<'a>, &'a FNtn<'a>> {
    let m = p.open_with(LBRACK);
    let mut vals = p.new_vec();
    while p.at_any(ARG_START) {
        vals.push(get(term(p)));
        // we use at_free here so that when we are unwinding a bunch of
        // recursive calls to arg, we don't use up all the gas
        if p.at_free(COMMA) {
            p.advance();
        } else {
            break;
        }
    }
    p.eat_free(m, RBRACK)?;
    Ok(p.close(m, Tuple(vals)))
}

fn block<'a>(p: &P<'a>) -> PResult<'a> {
    let m = p.open_with(LCURLY);
    let mut tms = p.new_vec();
    while p.at_any(ARG_START) {
        let t = get(term(p));
        // we use at_free here so that when we are unwinding a bunch of
        // recursive calls to arg, we don't use up all the gas
        if p.at_free(SEMICOLON) {
            tms.push(t);
            p.advance();
        } else {
            p.eat_free(m, RCURLY)?;
            return Ok(p.close(m, Block(tms, Some(t))));
        }
    }
    p.eat_free(m, RCURLY)?;
    return Ok(p.close(m, Block(tms, None)));
}

fn arg<'a>(p: &P<'a>, following: bool) -> PResult<'a> {
    assert!(p.at_any(ARG_START));
    let m = p.open();
    let mut t = match p.cur() {
        LPAREN => parens(p, term),
        LBRACK => tuple(p),
        LCURLY => block(p),
        VAR => Ok(p.advance_close(m, Var(p.slice()))),
        KEYWORD => Ok(p.advance_close(m, Keyword(p.slice()))),
        OP => Ok(p.advance_close(m, Var(p.slice()))),
        KEYWORD_OP => Ok(p.advance_close(m, Keyword(p.slice()))),
        INT => match p.slice().parse() {
            Ok(i) => Ok(p.advance_close(m, Int(i))),
            Err(e) => {
                p.advance();
                Err(error!(p, m, "could not parse int: {e}"))
            }
        },
        FLOAT => match p.slice().parse() {
            Ok(x) => Ok(p.advance_close(m, Float(x))),
            Err(e) => {
                p.advance();
                Err(error!(p, m, "could not parse float: {e}"))
            }
        },
        STRING => {
            let s = p.slice();
            Ok(p.advance_close(m, Str(&s[1..s.len() - 1])))
        }
        PRIM => {
            let name = &p.slice()[1..];
            Ok(p.advance_close(m, Prim(name)))
        }
        SPECIAL => {
            let name = &p.slice()[1..];
            Ok(p.advance_close(m, Special(name)))
        }
        FIELD => {
            let name = &p.slice()[1..];
            Ok(p.advance_close(m, Field(name)))
        }
        TAG => {
            let name = &p.slice()[1..];
            Ok(p.advance_close(m, Tag(name)))
        }
        t => {
            let e = error!(p, m, "expected start of term, found {:?}", t);
            p.advance();
            Err(e)
        }
    }?;
    // apply this to any arguments that don't have an intervening whitespace
    if !following {
        while p.at_any(ARG_START) && !p.at_any(&[OP, KEYWORD_OP]) && p.no_preceding_whitespace() {
            t = p.close(m, App1(t, arg(p, true)?));
        }
    }
    Ok(t)
}

fn term<'a>(p: &P<'a>) -> PResult<'a> {
    let m = p.open();
    if p.at(EOF) {
        return Err(error!(p, m, "empty term"));
    }
    if !p.at_any(ARG_START) {
        return Err(error!(p, m, "{:?} does not start a term argument", p.cur()));
    }
    let first_arg = arg(p, false)?;
    if !p.at_any(ARG_START) || p.at_any(&[OP, KEYWORD_OP]) {
        return Ok(first_arg);
    }
    let mut stack = TermStack::new(m, p);
    stack.push_term(p, first_arg);
    while !p.at(EOF) {
        if p.at_any(ARG_START) && !p.at_any(&[OP, KEYWORD_OP]) {
            stack.push_term(p, get(arg(p, false)));
        } else {
            let op_m = p.open();
            let op = match p.cur() {
                token::OP | token::KEYWORD_OP => {
                    let name = p.slice();
                    let expr = p.advance_close(
                        op_m,
                        if p.cur() == token::OP {
                            Var(name)
                        } else {
                            Keyword(name)
                        },
                    );
                    let Some(prec) = p.prec(name) else {
                        return Err(error!(p, m, "could not find precedence of {}", name));
                    };
                    BinOp { expr, prec }
                }
                _ => break,
            };
            stack.push_binop(p, op)?;
        }
    }
    Ok(stack.finish(p))
}

pub fn decl<'a>(start: Marker, annotations: &'a [&'a FNtn<'a>], p: &P<'a>) -> Option<FNtnTop<'a>> {
    let m = p.open();
    let s = p.slice();
    if let Err(_) = p.eat(m, TOPDECL) {
        p.advance(); // don't get stuck in loop!
        return None;
    };
    let t = get(term(p));
    Some(FNtnTop::new(annotations, s, p.loc_from(start), t))
}

pub fn parse_term<'a>(
    src: &'a str,
    reporter: Reporter,
    prectable: &'a HashMap<String, Prec>,
    tokens: &'a [Token],
    arena: &'a Bump,
) -> &'a FNtn<'a> {
    let p = Parser::new(src, reporter, prectable, tokens, arena);
    assert!(p.at(BOF));
    p.advance();
    get(term(&p))
}

pub fn parse_top<'a>(
    src: &'a str,
    reporter: Reporter,
    prectable: &'a HashMap<String, Prec>,
    tokens: &'a [Token],
    arena: &'a Bump,
) -> Vec<FNtnTop<'a>> {
    let mut out = Vec::new();
    let p = Parser::new(src, reporter, prectable, tokens, arena);
    assert!(p.at(BOF));
    p.advance();
    let mut annotations = BumpVec::new_in(arena);
    let start = p.open();
    while !p.at(EOF) {
        if p.at(ANNOT) {
            let m = p.open();
            p.advance();
            let n = get(term(&p));
            let _ = p.eat(m, RPAREN);
            annotations.push(n);
        }
        if let Some(t) = decl(
            start,
            std::mem::replace(&mut annotations, BumpVec::new_in(arena)).into_bump_slice(),
            &p,
        ) {
            out.push(t)
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use bumpalo::Bump;
    use expect_test::{expect, Expect};
    use indoc::indoc;

    use crate::{lexer::lex, ParseConfig};

    use tattle::{display::SourceInfo, Reporter};

    use super::{parse_term, parse_top, Prec};

    const DEMO_PARSECONFIG: ParseConfig = ParseConfig::new(
        &[
            ("=", Prec::lassoc(10)),
            (":", Prec::lassoc(20)),
            ("+", Prec::lassoc(50)),
            ("-", Prec::lassoc(50)),
            ("*", Prec::lassoc(60)),
            ("/", Prec::lassoc(60)),
        ],
        &["=", "E"],
        &["def"],
    );

    fn test(input: &str, expected: Expect) {
        let reporter = Reporter::new();
        let prectable: HashMap<_, _> = DEMO_PARSECONFIG
            .precedences
            .iter()
            .map(|(name, p)| (name.to_string(), *p))
            .collect();
        let tokens = lex(&input, &DEMO_PARSECONFIG, reporter.clone()).unwrap();
        let arena = Bump::new();
        let ast = parse_term(&input, reporter.clone(), &prectable, &tokens, &arena);
        reporter.info(format!("{}", ast));
        expected.assert_eq(&SourceInfo::new(None, input).extract_report_to_string(reporter));
    }

    fn test_top(input: &str, expected: Expect) {
        let reporter = Reporter::new();
        let prectable: HashMap<_, _> = DEMO_PARSECONFIG
            .precedences
            .iter()
            .map(|(name, p)| (name.to_string(), *p))
            .collect();
        let tokens = lex(&input, &DEMO_PARSECONFIG, reporter.clone()).unwrap();
        let arena = Bump::new();
        let toplevel = parse_top(&input, reporter.clone(), &prectable, &tokens, &arena);
        for topntn in toplevel.iter() {
            for annot in topntn.annotations {
                reporter.info(format!("annot: {}", annot));
            }
            reporter.info(format!("{} {}", topntn.name, topntn.body))
        }
        expected.assert_eq(&SourceInfo::new(None, input).extract_report_to_string(reporter));
    }

    #[test]
    fn grammar_tests() {
        test(
            "1 + 1",
            expect![[r#"
            info: 1 + 1
        "#]],
        );
        test(
            "",
            expect![[r#"
                error[syntax]: empty term
                --> <none>:1:1
                1|
                1|
                info: !!!
            "#]],
        );
        test(
            "E a",
            expect![[r#"
                info: E a
            "#]],
        );
        test(
            "{ a = 1; b = a + 4; 2 }",
            expect![[r#"
                info: { a = 1; b = (a + 4); 2 }
            "#]],
        );
        test(
            "{ Option (a: @type) : @type = %enum[ 'some a, 'none ]; }",
            expect![[r#"
                info: { ((Option (a : @type)) : @type) = (%enum ['some a, 'none]); }
            "#]],
        );
        test(
            "f x .b",
            expect![[r#"
            info: f x .b
        "#]],
        );
        test(
            "f x.b",
            expect![[r#"
            info: f (x .b)
        "#]],
        );
        test(
            "f @alloc[1]",
            expect![[r#"
            info: f (@alloc [1])
        "#]],
        );
        test(
            "f @alloc [1]",
            expect![[r#"
            info: f @alloc [1]
        "#]],
        );
        test(
            "a.1 * b.2 + c.1",
            expect![[r#"
            info: ((a .1) * (b .2)) + (c .1)
        "#]],
        );
        test(
            "1 + +",
            expect![[r#"
                error[syntax]: expected term after binary op
                --> <none>:1:1
                1| 1 + +
                1| ^^^^^
                error[syntax]: uncompleted term
                --> <none>:1:1
                1| 1 + +
                1| ^^^^^
                info: !!!
            "#]],
        );
        test_top(
            "def f x = x * x",
            expect![[r#"
                info: def (f x) = (x * x)
            "#]],
        );
        test_top(
            indoc! {r#"
                def f x = x * x

                #/ a comment

                #(annotation)
                def g x = f (f x)
            "#},
            expect![[r#"
                info: def (f x) = (x * x)
                info: annot: annotation
                info: def (g x) = (f (f x))
            "#]],
        )
    }
}
