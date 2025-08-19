use crate::{token::*, ParseConfig};
use std::{iter::Peekable, str::Chars};
use tattle::{declare_error, Loc, Reporter};

const OPERATOR_CHARS: &'static [char] =
    &['+', '*', '-', '/', '<', '>', '&', '|', '!', '=', ':', '↦'];

declare_error!(LEX_ERROR, "lex", "an error during the lexing phase");

struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    source: &'a str,
    reporter: Reporter,
    parse_config: &'a ParseConfig<'a>,
    out: Vec<Token>,
    preceding_whitespace: bool,
    prev: usize,
    cur: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, parse_config: &'a ParseConfig, reporter: Reporter) -> Self {
        Lexer {
            iter: source.chars().peekable(),
            source,
            reporter,
            parse_config,
            out: Vec::new(),
            preceding_whitespace: false,
            prev: 0,
            cur: 0,
        }
    }

    fn error(&mut self, message: String) {
        let l = Loc::new(self.prev, self.cur);
        self.out
            .push(Token::new(self.preceding_whitespace, ERROR, l));
        self.reporter.error(l, LEX_ERROR, message.into());
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().copied()
    }

    fn advance(&mut self) -> Option<char> {
        self.iter.next().map(|c| {
            self.cur += c.len_utf8();
            c
        })
    }

    fn emit(&mut self, kind: Kind) {
        self.out.push(Token::new(
            self.preceding_whitespace,
            kind,
            Loc::new(self.prev, self.cur),
        ));
        self.preceding_whitespace = false;
        self.prev = self.cur;
    }

    fn skip(&mut self) {
        self.prev = self.cur;
    }

    fn slice(&self) -> &'a str {
        &self.source[self.prev..self.cur]
    }

    fn many<F: Fn(char) -> bool>(&mut self, f: F) {
        while let Some(c) = self.peek() {
            if f(c) {
                self.advance();
            } else {
                break;
            }
        }
    }
}

macro_rules! error {
    ($m:expr, $msg:literal) => {{
        $m.error(format!($msg));
    }};
    ($m:expr, $msg:literal, $($arg:expr),+) => {{
        $m.error(format!($msg, $($arg),+));
    }};
}

fn word(l: &mut Lexer) {
    l.many(|c| c.is_alphanumeric() || c == '_');
}

fn keyword(l: &mut Lexer, kind: Kind) {
    word(l);
    l.emit(kind);
}

fn op(l: &mut Lexer, as_var: bool) {
    l.many(|c| OPERATOR_CHARS.contains(&c));
    if l.parse_config.is_keyword(l.slice()) {
        l.emit(if as_var { KEYWORD } else { KEYWORD_OP });
    } else {
        l.emit(if as_var { VAR } else { OP });
    }
}

fn num(l: &mut Lexer) {
    l.many(|c| c.is_digit(10));
    if l.peek() == Some('.') {
        l.advance();
        l.many(|c| c.is_digit(10));
        l.emit(FLOAT);
    } else {
        l.emit(INT);
    }
}

fn string(l: &mut Lexer) {
    l.many(|c| c != '"');
    l.advance();
    l.emit(STRING);
}

fn run(l: &mut Lexer) {
    l.emit(BOF);
    while let Some(c) = l.advance() {
        match c {
            _ if c.is_whitespace() => {
                l.skip();
                l.preceding_whitespace = true;
                continue;
            }
            _ if c.is_alphabetic() || c == '_' => {
                word(l);
                if l.parse_config.is_keyword(l.slice()) {
                    l.emit(KEYWORD);
                } else if l.parse_config.is_toplevel(l.slice()) {
                    l.emit(TOPDECL)
                } else {
                    l.emit(VAR);
                }
            }
            _ if c.is_digit(10) => num(l),
            _ if OPERATOR_CHARS.contains(&c) => op(l, false),
            '#' => {
                while let Some(c) = l.advance() {
                    if c == '\n' {
                        break;
                    }
                }
            }
            '`' => {
                l.skip();
                op(l, true)
            }
            '@' => keyword(l, PRIM),
            '%' => keyword(l, SPECIAL),
            '.' => keyword(l, FIELD),
            '\'' => keyword(l, TAG),
            '"' => string(l),
            ';' => l.emit(SEMICOLON),
            ',' => l.emit(COMMA),
            '{' => l.emit(LCURLY),
            '}' => l.emit(RCURLY),
            '(' => l.emit(LPAREN),
            ')' => l.emit(RPAREN),
            '[' => l.emit(LBRACK),
            ']' => l.emit(RBRACK),
            _ => {
                error!(l, "unexpected character");
            }
        }
    }
}

pub fn lex(source: &str, parse_config: &ParseConfig, reporter: Reporter) -> Vec<Token> {
    let mut lexer = Lexer::new(source, parse_config, reporter);
    run(&mut lexer);
    lexer.out
}

#[cfg(test)]
mod test {
    use expect_test::{expect, Expect};

    use crate::ParseConfig;

    use super::lex;

    use tattle::{display::SourceInfo, Reporter};

    const DEMO_PARSECONFIG: ParseConfig = ParseConfig::new(&[], &[], &[]);

    fn test(input: &str, expected: Expect) {
        let reporter = Reporter::new();
        let tokens = lex(&input, &DEMO_PARSECONFIG, reporter.clone());
        reporter.info(format!("{:?}", tokens));
        expected.assert_eq(&SourceInfo::new(None, input).extract_report_to_string(reporter));
    }

    #[test]
    fn lexer_tests() {
        test(
            "E",
            expect![[r#"
                info: [Token { preceding_whitespace: false, kind: BOF, loc: Loc { start: 0, end: 0 } }, Token { preceding_whitespace: false, kind: VAR, loc: Loc { start: 0, end: 1 } }]
            "#]],
        );
        test(
            "A",
            expect![[r#"
            info: [Token { preceding_whitespace: false, kind: BOF, loc: Loc { start: 0, end: 0 } }, Token { preceding_whitespace: false, kind: VAR, loc: Loc { start: 0, end: 1 } }]
        "#]],
        );
    }
}
