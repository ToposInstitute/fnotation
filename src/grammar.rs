use std::collections::HashMap;

use crate::parser::*;
use crate::token::{self, *};
use crate::types::*;
use bumpalo::Bump;
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
    VAR, KEYWORD, INT, FLOAT, LPAREN, LBRACK, LCURLY, STRING, PRIM, SPECIAL, TAG, FIELD,
];

type P<'a> = Parser<'a>;

fn parens<'a, F: FnMut(&P<'a>) -> PResult<'a>>(p: &P<'a>, mut f: F) -> PResult<'a> {
    let m = p.open_with(LPAREN);
    let s = f(p);
    p.eat(m, RPAREN)?;
    Ok(s?)
}

fn tuple<'a>(p: &P<'a>) -> Result<&'a FExp<'a>, &'a FExp<'a>> {
    let m = p.open_with(LBRACK);
    let mut vals = p.new_vec();
    while p.at_any(ARG_START) {
        vals.push(get(term(p)));
        if p.at(COMMA) {
            p.advance();
        } else {
            break;
        }
    }
    p.eat(m, RBRACK)?;
    Ok(p.close(m, Tuple(vals)))
}

fn block<'a>(p: &P<'a>) -> PResult<'a> {
    let m = p.open_with(LCURLY);
    let mut tms = p.new_vec();
    while p.at_any(ARG_START) {
        let t = get(term(p));
        if p.at(SEMICOLON) {
            tms.push(t);
            p.advance();
        } else {
            p.eat(m, RCURLY)?;
            return Ok(p.close(m, Block(tms, Some(t))));
        }
    }
    p.eat(m, RCURLY)?;
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
        INT => Ok(p.advance_close(m, Int(p.slice().parse().unwrap()))),
        FLOAT => Ok(p.advance_close(m, Float(p.slice().parse().unwrap()))),
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
        t => Err(error!(p, m, "expected start of term, found {:?}", t)),
    }?;
    // apply this to any arguments that don't have an intervening whitespace
    if !following {
        while p.at_any(ARG_START) && p.no_preceding_whitespace() {
            t = p.close(m, App1(t, arg(p, true)?));
        }
    }
    Ok(t)
}

fn term<'a>(p: &P<'a>) -> PResult<'a> {
    let m = p.open();
    let mut stack = TermStack::new(m, p);
    loop {
        if p.at_any(ARG_START) {
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

pub fn parse<'a>(
    src: &'a str,
    reporter: Reporter,
    prectable: &'a HashMap<String, Prec>,
    tokens: &'a [Token],
    arena: &'a Bump,
) -> &'a FExp<'a> {
    let p = Parser::new(src, reporter, prectable, tokens, arena);
    get(term(&p))
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::rc::Rc;

    use bumpalo::Bump;
    use expect_test::{expect, Expect};

    use crate::lexer::lex;

    use tattle::Reporter;

    use super::{parse, Prec};
    const DEMO_PRECTABLE: &[(&str, Prec)] = &[
        ("=", Prec::lassoc(10)),
        (":", Prec::lassoc(20)),
        ("+", Prec::lassoc(50)),
        ("-", Prec::lassoc(50)),
        ("*", Prec::lassoc(60)),
        ("/", Prec::lassoc(60)),
    ];
    const DEMO_KEYWORDTABLE: &[&str] = &["="];

    fn test(input: &str, expected: Expect) {
        let reporter = Reporter::new(Rc::new(input.to_string()));
        let prectable: HashMap<_, _> = DEMO_PRECTABLE
            .iter()
            .map(|(name, p)| (name.to_string(), *p))
            .collect();
        let tokens = lex(
            input,
            DEMO_KEYWORDTABLE.iter().map(|s| s.to_string()).collect(),
            reporter.clone(),
        );
        let arena = Bump::new();
        let ast = parse(input, reporter.clone(), &prectable, &tokens, &arena);
        reporter.info(format!("{}", ast));
        expected.assert_eq(&reporter.report());
    }

    #[test]
    fn grammar_tests() {
        test("1 + 1", expect!["1 + 1"]);
        test(
            "{ a = 1; b = a + 4; 2 }",
            expect!["{ a = 1; b = (a + 4); 2 }"],
        );
        test(
            "{ Option (a: @type) : @type = %enum[ 'some a, 'none ]; }",
            expect!["{ ((Option (a : @type)) : @type) = (%enum ['some a, 'none]); }"],
        );
        test("f x .b", expect!["f x .b"]);
        test("f x.b", expect!["f (x .b)"]);
        test("f @alloc[1]", expect!["f (@alloc [1])"]);
        test("f @alloc [1]", expect!["f @alloc [1]"]);
        test("a.1 * b.2 + c.1", expect!["((a .1) * (b .2)) + (c .1)"]);
    }
}
