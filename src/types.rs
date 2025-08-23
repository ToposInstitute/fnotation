use std::fmt;

use bumpalo::collections::Vec;
use tattle::Loc;

#[derive(PartialEq, Debug)]
pub enum FNtn<'a> {
    L(Loc, FNtn0<'a>),
}

pub use FNtn::L;

impl<'a> FNtn<'a> {
    pub fn loc(&self) -> Loc {
        match self {
            Self::L(l, _) => *l,
        }
    }

    pub fn ast0(&self) -> &FNtn0<'a> {
        match self {
            Self::L(_, s) => s,
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum IsInfix {
    Yes,
    No,
}

#[derive(PartialEq, Debug)]
pub enum FNtn0<'a> {
    /// `f x => App1(f, x)`
    App1(&'a FNtn<'a>, &'a FNtn<'a>),

    /// `x + y => App2(f, x, y)
    App2(&'a FNtn<'a>, &'a FNtn<'a>, &'a FNtn<'a>),

    /// `a => Var("a")`
    Var(&'a str),
    /// `= => Keyword("=")`
    Keyword(&'a str),

    /// `[x, y] => Tuple([x, y])`
    Tuple(Vec<'a, &'a FNtn<'a>>),

    /// `{ x; y; } => Block([x, y], None)`
    /// `{ x; y; e } => Block([x, y], Some(e))`
    Block(Vec<'a, &'a FNtn<'a>>, Option<&'a FNtn<'a>>),

    /// `3 => Int(3)`
    Int(u64),
    /// `3.0 => Float(3.0)`
    Float(f64),
    /// "hello world" => Str("hello world")
    Str(&'a str),

    /// `@foo => Prim("foo")`
    Prim(&'a str),
    /// `%bar => Special("bar")`
    Special(&'a str),
    /// `.first => Field("first")`
    Field(&'a str),
    /// `'some => Tag("some")`
    Tag(&'a str),

    /// It may be possible to keep parsing after an error; when this happens we
    /// insert an Error node into the syntax tree where the error happened
    Error,
}

pub struct FNtnTop<'a> {
    pub annotations: &'a [&'a FNtn<'a>],
    pub name: &'a str,
    pub loc: Loc,
    pub body: &'a FNtn<'a>,
}

impl<'a> FNtnTop<'a> {
    pub fn new(
        annotations: &'a [&'a FNtn<'a>],
        name: &'a str,
        loc: Loc,
        body: &'a FNtn<'a>,
    ) -> Self {
        Self {
            annotations,
            name,
            loc,
            body,
        }
    }
}

pub use FNtn0::*;

use pretty::RcDoc;

fn bexpr<'a>(args: &'a [&'a FNtn<'a>]) -> RcDoc<'a> {
    RcDoc::text("[")
        .append(
            RcDoc::intersperse(
                args.iter().map(|x| x.to_doc()),
                RcDoc::text(",").append(RcDoc::line()),
            )
            .nest(1)
            .group(),
        )
        .append(RcDoc::text("]"))
}

impl<'a> FNtn<'a> {
    fn parens(&self) -> RcDoc {
        let should_parenthesize = match self.ast0() {
            App1(_, _) | App2(_, _, _) => true,
            _ => false,
        };
        if should_parenthesize {
            RcDoc::text("(")
                .append(self.to_doc().nest(1).group())
                .append(RcDoc::text(")"))
        } else {
            self.to_doc()
        }
    }

    pub fn collect_args(&'a self) -> (&'a FNtn<'a>, std::vec::Vec<&'a FNtn<'a>>) {
        let mut revargs = std::vec::Vec::new();
        let mut cur = self;
        loop {
            match cur.ast0() {
                App1(f, x) => {
                    cur = f;
                    revargs.push(*x);
                }
                _ => break,
            }
        }
        revargs.reverse();
        (cur, revargs)
    }

    fn to_doc(&self) -> RcDoc {
        match self.ast0() {
            App1(f, arg) => f
                .to_doc()
                .append(RcDoc::line())
                .append(arg.parens())
                .group(),
            App2(f, l, r) => l
                .parens()
                .append(RcDoc::line())
                .append(f.to_doc())
                .append(RcDoc::line())
                .append(r.parens())
                .group(),
            Prim(name) => RcDoc::text(format!("@{}", name)),
            Special(name) => RcDoc::text(format!("%{}", name)),
            Tag(name) => RcDoc::text(format!("'{}", name)),
            Field(name) => RcDoc::text(format!(".{}", name)),
            Tuple(args) => bexpr(args),
            Int(i) => RcDoc::text(format!("{}", i)),
            Float(x) => RcDoc::text(format!("{}", x)),
            Str(s) => RcDoc::text(format!("\"{}\"", s)),
            Var(s) => RcDoc::text(s.to_string()),
            Keyword(s) => RcDoc::text(s.to_string()),
            Block(stmts, res) => RcDoc::text("{")
                .append(RcDoc::line())
                .append(RcDoc::intersperse(
                    stmts.iter().map(|x| x.to_doc().append(RcDoc::text(";"))),
                    RcDoc::line(),
                ))
                .append(RcDoc::line())
                .append(
                    res.map(|e| e.to_doc().append(RcDoc::line()))
                        .unwrap_or(RcDoc::nil()),
                )
                .append(RcDoc::text("}"))
                .group(),
            Error => RcDoc::text("!!!"),
        }
    }
}

impl<'a> fmt::Display for FNtn<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_doc().render_fmt(100, f)
    }
}
