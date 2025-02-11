use crate::token::*;
use std::{fmt, iter::Peekable, str::Chars};
use tattle::{declare_error, Loc, Reporter};

const OPERATOR_CHARS: &'static [char] =
    &['+', '*', '-', '/', '<', '>', '&', '|', '!', '=', ':', '↦'];

declare_error!(LEX_ERROR, "lex", "an error during the lexing phase");

struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    source: &'a str,
    reporter: Reporter,
    keywords: &'a [&'a str],
    out: Vec<Token>,
    preceding_whitespace: bool,
    prev: usize,
    cur: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, keywords: &'a [&'a str], reporter: Reporter) -> Self {
        Lexer {
            iter: source.chars().peekable(),
            source,
            reporter,
            keywords,
            out: Vec::new(),
            preceding_whitespace: false,
            prev: 0,
            cur: 0,
        }
    }

    fn error<W: Fn(&mut fmt::Formatter) -> fmt::Result>(&mut self, writer: W) {
        let l = Loc::new(self.prev, self.cur, None);
        self.out
            .push(Token::new(self.preceding_whitespace, ERROR, l));
        self.reporter.error(l, LEX_ERROR, writer);
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
            Loc::new(self.prev, self.cur, None),
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
        $m.error(|f| write!(f, $msg));
    }};
    ($m:expr, $msg:literal, $($arg:expr),+) => {{
        $m.error(|f| write!(f, $msg, $($arg),+));
    }};
}

fn keyword(l: &mut Lexer, kind: Kind) {
    l.many(|c| c.is_alphanumeric());
    l.emit(kind);
}

fn op(l: &mut Lexer) {
    l.many(|c| OPERATOR_CHARS.contains(&c));
    if l.keywords.contains(&l.slice()) {
        l.emit(KEYWORD_OP);
    } else {
        l.emit(OP);
    }
}

fn int(l: &mut Lexer) {
    l.many(|c| c.is_digit(10));
    l.emit(INT);
}

fn string(l: &mut Lexer) {
    l.many(|c| c != '"');
    l.advance();
    l.emit(STRING);
}

fn run(l: &mut Lexer) {
    while let Some(c) = l.advance() {
        match c {
            _ if c.is_whitespace() => {
                l.skip();
                l.preceding_whitespace = true;
                continue;
            }
            _ if c.is_alphabetic() => {
                l.many(|c| c.is_alphanumeric());
                l.emit(VAR);
            }
            _ if c.is_digit(10) => int(l),
            _ if OPERATOR_CHARS.contains(&c) => op(l),
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
            '_' => l.emit(UNDERSCORE),
            _ => {
                error!(l, "unexpected character");
            }
        }
    }
}

pub fn lex(source: &str, keywords: &[&str], reporter: Reporter) -> Vec<Token> {
    let mut lexer = Lexer::new(source, keywords, reporter);
    run(&mut lexer);
    lexer.out
}
