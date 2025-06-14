use tattle::Loc;

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Kind {
    ERROR,
    EOF,

    VAR,
    KEYWORD,
    PRIM,
    SPECIAL,
    FIELD,
    OP,
    KEYWORD_OP,
    TAG,

    INT,
    FLOAT,
    STRING,

    LPAREN,
    RPAREN,
    LCURLY,
    RCURLY,
    LBRACK,
    RBRACK,
    COMMA,
    SEMICOLON,
}

pub use Kind::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Token {
    pub preceding_whitespace: bool,
    pub kind: Kind,
    pub loc: Loc,
}

impl Token {
    pub fn new(preceding_whitespace: bool, kind: Kind, loc: Loc) -> Self {
        Self {
            preceding_whitespace,
            kind,
            loc,
        }
    }
}
