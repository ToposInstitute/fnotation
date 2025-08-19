use bumpalo::Bump;
use tattle::Reporter;

use crate::{
    grammar::{parse_term, parse_top},
    lexer::lex,
    parser::Prec,
    FNtn, FNtnTop,
};

pub struct ParseConfig<'a> {
    pub precedences: &'a [(&'a str, Prec)],
    pub keywords: &'a [&'a str],
    pub toplevels: &'a [&'a str],
}

impl<'a> ParseConfig<'a> {
    pub const fn new(
        precedences: &'a [(&'a str, Prec)],
        keywords: &'a [&'a str],
        toplevels: &'a [&'a str],
    ) -> Self {
        Self {
            precedences,
            keywords,
            toplevels,
        }
    }

    pub fn is_keyword(&self, s: &str) -> bool {
        self.keywords.contains(&s)
    }

    pub fn is_toplevel(&self, s: &str) -> bool {
        self.toplevels.contains(&s)
    }

    pub fn with_parsed<A, F: FnOnce(&FNtn) -> Option<A>>(
        &self,
        input: &str,
        reporter: Reporter,
        f: F,
    ) -> Option<A> {
        let precedences = self
            .precedences
            .iter()
            .map(|(s, p)| (s.to_string(), *p))
            .collect();
        let tokens = lex(input, &self, reporter.clone());
        let arena = Bump::new();
        let ast = parse_term(input, reporter.clone(), &precedences, &tokens, &arena);
        if reporter.errored() {
            None
        } else {
            f(ast)
        }
    }

    pub fn with_parsed_top<A, F: FnOnce(&[FNtnTop]) -> Option<A>>(
        &self,
        input: &str,
        reporter: Reporter,
        f: F,
    ) -> Option<A> {
        let precedences = self
            .precedences
            .iter()
            .map(|(s, p)| (s.to_string(), *p))
            .collect();
        let tokens = lex(input, &self, reporter.clone());
        let arena = Bump::new();
        let top = parse_top(input, reporter.clone(), &precedences, &tokens, &arena);
        if reporter.errored() {
            None
        } else {
            f(&top)
        }
    }
}
