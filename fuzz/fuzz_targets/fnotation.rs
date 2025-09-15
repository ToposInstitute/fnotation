#![no_main]

use bumpalo::Bump;
use fnotation::grammar::parse_term;
use fnotation::lexer::lex;
use fnotation::parser::Prec;
use fnotation::*;
use libfuzzer_sys::fuzz_target;
use std::io;
use tattle::display::SourceInfo;
use tattle::*;

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

fuzz_target!(|data: &[u8]| {
    let Ok(input) = str::from_utf8(data) else {
        return;
    };
    let reporter = Reporter::new();
    println!("{}", input);
    println!("{}", input.len());
    let precedences = DEMO_PARSECONFIG
        .precedences
        .iter()
        .map(|(s, p)| (s.to_string(), *p))
        .collect();
    let source_info = SourceInfo::new(Some("<fuzz input>"), &input);
    let Ok(tokens) = lex(input, &DEMO_PARSECONFIG, reporter.clone()) else {
        source_info
            .extract_report_to_io(
                &mut io::stdout(),
                reporter.clone(),
                tattle::display::DisplayOptions::String,
            )
            .unwrap();
        return;
    };
    source_info
        .extract_report_to_io(
            &mut io::stdout(),
            reporter.clone(),
            tattle::display::DisplayOptions::String,
        )
        .unwrap();
    for tok in tokens.iter() {
        print!("{} ", tok);
    }
    println!();
    let arena = Bump::new();
    let _ast = parse_term(input, reporter.clone(), &precedences, &tokens, &arena);
    source_info
        .extract_report_to_io(
            &mut io::stdout(),
            reporter.clone(),
            tattle::display::DisplayOptions::String,
        )
        .unwrap();
});
