# F-notation

[documentation](https://forest.topos.site/public/ocl-00CE)

[example of use](https://github.com/ToposInstitute/emtt/tree/main/examples)

F-notation is a [lower house](https://parentheticallyspeaking.org/articles/bicameral-not-homoiconic/) syntax for programming language experimentation. The idea is that it's a slightly richer version of s-expressions that can be used as the base for many different programming languages. The "f" stands for either **f**lexible or **f**unctional, or any other flattering word that starts with "f". This is a library for parsing f-notation.

This package used to be called `fexplib`, but "f-expression" has a [pre-existing meaning](https://en.wikipedia.org/wiki/Fexpr). It seems like fnotation is a thing in Prolog (meaning "function notation"), but I think this isn't too well-established.

Another reason for the change is that "notation" is a better word for "a collection of conventions for representing a formal language in textual form", while "expression" really refers to the syntactic class of bits of an abstract syntax tree that can be evaluated to a value (vis. the expression vs. statement distinction).

This is the Rust implementation of f-notation. At some point I might make some implementations in other languages and add them to this repository as well.

## Inspirations

- lisp
- [Julia](https://julialang.org)
- [narya](https://github.com/mikeshulman/narya/)
- [rhombus](https://docs.racket-lang.org/rhombus/index.html)
