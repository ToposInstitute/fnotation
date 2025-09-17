use crate::token::{self, *};
use crate::types::*;

use bumpalo::collections::Vec;
use bumpalo::Bump;
use std::cell::Cell;
use std::collections::HashMap;
use tattle::{declare_error, Loc, Reporter};

macro_rules! error {
    ($p:expr, $m:expr, $msg:literal) => {{
        $p.error($m, None, format!($msg))
    }};
    ($p:expr, $m:expr, $msg:literal, $($arg:expr),+) => {{
        $p.error($m, None, format!($msg, $($arg),+))
    }};
}

macro_rules! error_at {
    ($p:expr, $m:expr, $l:expr, $msg:literal) => {{
        $p.error($m, Some($l), format!($msg))
    }};
    ($p:expr, $m:expr, $l:expr, $msg:literal, $($arg:expr),+) => {{
        $p.error($m, Some($l), format!($msg, $($arg),+))
    }};
}

declare_error!(SYNTAX_ERROR, "syntax", "an error during the parsing phase");

pub type Marker = usize;

pub struct Parser<'a> {
    src: &'a str,
    reporter: Reporter,
    tokens: &'a [Token],
    arena: &'a Bump,
    prectable: &'a HashMap<String, Prec>,
    pos: Cell<usize>,
    gas: Cell<usize>,
}

impl<'a> Parser<'a> {
    pub fn new(
        src: &'a str,
        reporter: Reporter,
        prectable: &'a HashMap<String, Prec>,
        tokens: &'a [Token],
        arena: &'a Bump,
    ) -> Self {
        Parser {
            src,
            reporter,
            tokens,
            arena,
            prectable,
            pos: Cell::new(0),
            gas: Cell::new(256),
        }
    }

    pub fn cur(&self) -> Kind {
        if self.gas.get() == 0 {
            println!("probable infinite loop in grammar, stuck after parsing:");
            for tok in self.tokens[0..self.pos.get()].iter() {
                print!("{} ", tok);
            }
            println!();
            panic!();
        }

        self.gas.set(self.gas.get() - 1);
        if self.pos.get() >= self.tokens.len() {
            token::EOF
        } else {
            self.tokens[self.pos.get()].kind
        }
    }

    pub fn no_preceding_whitespace(&self) -> bool {
        if self.pos.get() >= self.tokens.len() {
            false
        } else {
            !self.tokens[self.pos.get()].preceding_whitespace
        }
    }

    pub fn advance(&self) {
        self.gas.set(256);
        self.pos.set(self.pos.get() + 1);
    }

    pub fn open(&self) -> Marker {
        self.tokens[self.pos.get().min(self.tokens.len() - 1)]
            .loc
            .start
    }

    fn closing_pos(&self) -> Marker {
        let prev = self.pos.get().min(self.tokens.len()) - 1;
        self.tokens[prev].loc.end
    }

    pub fn loc_from(&self, m: Marker) -> Loc {
        let n = self.closing_pos();
        Loc::new(n.min(m), n)
    }

    pub fn close(&self, m: Marker, node: FNtn0<'a>) -> &'a FNtn<'a> {
        self.arena.alloc(FNtn::L(self.loc_from(m), node))
    }

    pub fn close_with(&self, m: Marker, e: Marker, node: FNtn0<'a>) -> &'a FNtn<'a> {
        self.arena.alloc(FNtn::L(Loc::new(m, e), node))
    }

    pub fn new_vec<T>(&self) -> Vec<'a, T> {
        Vec::new_in(self.arena)
    }

    pub fn slice(&self) -> &'a str {
        if self.pos.get() >= self.tokens.len() {
            ""
        } else {
            self.tokens[self.pos.get()].loc.slice(self.src)
        }
    }

    pub fn error(&self, m: Marker, loc: Option<Loc>, message: String) -> &'a FNtn<'a> {
        let loc = loc.unwrap_or_else(|| self.loc_from(m));
        self.reporter.error(loc, SYNTAX_ERROR, message);
        self.close(m, Error)
    }

    pub fn prec(&self, op: &str) -> Option<Prec> {
        self.prectable.get(op).copied()
    }

    pub fn at(&self, token: Kind) -> bool {
        self.cur() == token
    }

    /// Only use in situation we are sure it won't
    /// cause an infinite loop.
    pub fn cur_free(&self) -> Kind {
        if self.pos.get() >= self.tokens.len() {
            token::EOF
        } else {
            self.tokens[self.pos.get()].kind
        }
    }

    pub fn at_free(&self, token: Kind) -> bool {
        self.cur_free() == token
    }

    pub fn eat_free(&self, m: Marker, kind: Kind) -> Result<(), &'a FNtn<'a>> {
        let cur = self.cur_free();
        if cur == kind {
            self.advance();
            Ok(())
        } else {
            Err(error_at!(
                self,
                m,
                self.cur_loc(),
                "unexpected token {:?}, expected {:?}",
                cur,
                kind
            ))
        }
    }

    pub fn at_any(&self, tokens: &[Kind]) -> bool {
        tokens.contains(&self.cur())
    }

    pub fn cur_loc(&self) -> Loc {
        if self.pos.get() >= self.tokens.len() {
            Loc::new(self.src.len(), self.src.len() + 1)
        } else {
            self.tokens[self.pos.get()].loc
        }
    }

    pub fn eat(&self, m: Marker, kind: Kind) -> Result<(), &'a FNtn<'a>> {
        let cur = self.cur();
        if cur == kind {
            self.advance();
            Ok(())
        } else {
            Err(error_at!(
                self,
                m,
                self.cur_loc(),
                "unexpected token {:?}, expected {:?}",
                cur,
                kind
            ))
        }
    }

    pub fn open_with(&self, kind: Kind) -> Marker {
        assert!(self.at(kind));
        let m = self.open();
        self.advance();
        m
    }

    pub fn advance_close(&self, m: Marker, node: FNtn0<'a>) -> &'a FNtn<'a> {
        self.advance();
        self.close(m, node)
    }

    pub fn slice_when(&self, m: Marker, kind: Kind) -> Result<&'a str, &'a FNtn<'a>> {
        let s = self.slice();
        self.eat(m, kind).map(|_| s)
    }
}

pub type PResult<'a> = Result<&'a FNtn<'a>, &'a FNtn<'a>>;

pub fn get<'a>(r: PResult<'a>) -> &'a FNtn<'a> {
    r.unwrap_or_else(|s| s)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Assoc {
    Left,
    Right,
    Non,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Prec {
    tightness: usize,
    assoc: Assoc,
}

impl Prec {
    pub const fn lassoc(tightness: usize) -> Self {
        Prec {
            tightness,
            assoc: Assoc::Left,
        }
    }

    pub const fn rassoc(tightness: usize) -> Self {
        Prec {
            tightness,
            assoc: Assoc::Right,
        }
    }

    pub const fn nonassoc(tightness: usize) -> Self {
        Prec {
            tightness,
            assoc: Assoc::Non,
        }
    }

    fn loosest() -> Self {
        Prec {
            tightness: 0,
            assoc: Assoc::Non,
        }
    }
}

impl PartialOrd for Prec {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::*;
        use Assoc::*;
        match self.tightness.cmp(&other.tightness) {
            Less => Some(Less),
            Equal => match (self.assoc, other.assoc) {
                (Left, Left) => Some(Greater),
                (Right, Right) => Some(Less),
                _ => None,
            },
            Greater => Some(Greater),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct BinOp<'a> {
    pub expr: &'a FNtn<'a>,
    pub prec: Prec,
}

impl<'a> BinOp<'a> {
    fn start(&self) -> usize {
        self.expr.loc().start
    }

    fn app(&self, x: &'a FNtn<'a>, y: &'a FNtn<'a>) -> FNtn0<'a> {
        App2(self.expr, x, y)
    }
}

impl<'a> PartialOrd for BinOp<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.prec.partial_cmp(&other.prec)
    }
}

enum PrecState<'a> {
    Complete(&'a FNtn<'a>),
    HalfApp(&'a FNtn<'a>, BinOp<'a>),
    Error(usize), // start position of the error
}

pub struct TermStack<'a> {
    states: Vec<'a, PrecState<'a>>,
    start: usize,
}

impl<'a> TermStack<'a> {
    pub fn new(start: usize, p: &Parser<'a>) -> Self {
        TermStack {
            start,
            states: p.new_vec(),
        }
    }

    pub fn push_term(&mut self, p: &Parser<'a>, i: &'a FNtn<'a>) {
        use PrecState::*;
        match self.states.pop() {
            None => self.states.push(Complete(i)),
            Some(x) => match x {
                Complete(f) => self
                    .states
                    .push(Complete(p.close(f.loc().start, App1(f, i)))),
                HalfApp(l, op) => {
                    self.states.push(HalfApp(l, op));
                    self.states.push(Complete(i));
                }
                Error(i) => self.states.push(Error(i)),
            },
        }
    }

    pub fn collect_for(&mut self, p: &Parser<'a>, prec: Prec, mut i: &'a FNtn<'a>) -> &'a FNtn<'a> {
        use PrecState::*;
        while let Some(x) = self.states.pop() {
            match x {
                Complete(_j) => {
                    // this should never happen
                    panic!()
                }
                HalfApp(l, op) => {
                    if op.prec > prec {
                        i = p.close_with(l.loc().start, i.loc().end, op.app(l, i))
                    } else {
                        self.states.push(HalfApp(l, op));
                        return i;
                    }
                }
                Error(u) => {
                    self.states.push(Error(u));
                    return i;
                }
            }
        }
        i
    }

    pub fn push_binop(&mut self, p: &Parser<'a>, op: BinOp<'a>) -> Result<(), &'a FNtn<'a>> {
        use PrecState::*;
        match self.states.pop() {
            None => Err(error!(p, op.start(), "expected term before binary op"))?,
            Some(x) => match x {
                Complete(i) => {
                    let i = self.collect_for(p, op.prec, i);
                    self.states.push(HalfApp(i, op))
                }
                HalfApp(i, _prevop) => {
                    error!(p, i.loc().start, "expected term after binary op");
                    self.states.push(Error(i.loc().start))
                }
                Error(i) => self.states.push(Error(i)),
            },
        }
        Ok(())
    }

    pub fn finish(mut self, p: &Parser<'a>) -> &'a FNtn<'a> {
        use PrecState::*;
        if self.states.is_empty() {
            return error!(p, self.start, "uncompleted term");
        }
        let i = match self.states.pop().unwrap() {
            Complete(i) => self.collect_for(p, Prec::loosest(), i),
            _ => {
                return error!(p, self.start, "uncompleted term");
            }
        };
        if !self.states.is_empty() {
            return error!(p, self.start, "uncompleted term");
        } else {
            i
        }
    }
}
