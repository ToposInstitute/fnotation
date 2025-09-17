use std::fmt::Write;

use expect_test::Expect;

pub fn strip_lines(s: &str) -> String {
    let mut out = String::new();
    for l in s.lines() {
        write!(&mut out, "{}\n", l.trim_end()).unwrap();
    }
    out
}

pub trait AssertEqStripped {
    fn assert_eq_stripped(&self, actual: &str);
}

impl AssertEqStripped for Expect {
    fn assert_eq_stripped(&self, actual: &str) {
        let stripped = strip_lines(actual);
        self.assert_eq(&stripped);
    }
}
