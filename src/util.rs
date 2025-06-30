use bumpalo::Bump;
use tattle::Reporter;

use crate::{grammar::parse, lexer::lex, parser::Prec, FExp};

pub struct ParseConfig<'a> {
    precedences: &'a [(&'a str, Prec)],
    keywords: &'a [&'a str],
}

impl<'a> ParseConfig<'a> {
    pub const fn new(precedences: &'a [(&'a str, Prec)], keywords: &'a [&'a str]) -> Self {
        Self {
            precedences,
            keywords,
        }
    }

    pub fn with_parsed<A, F: FnOnce(&FExp) -> Option<A>>(
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
        let keywords = self.keywords.iter().map(|s| s.to_string()).collect();
        let tokens = lex(input, keywords, reporter.clone());
        let arena = Bump::new();
        let ast = parse(input, reporter.clone(), &precedences, &tokens, &arena);
        if reporter.errored() {
            None
        } else {
            f(ast)
        }
    }
}
