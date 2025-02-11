# Fexplib

F-expressions are a new [lower house](https://parentheticallyspeaking.org/articles/bicameral-not-homoiconic/) syntax for programming language experimentation. The "f" stands for either **f**lexible or **f**unctional, or any other flattering word that starts with "f". This is a library for parsing f-expressions.

## Principles

1. Juxtaposition is application (`f x y`). Note that `f[x,y]` is also valid syntax (it is the application of `f` to the tuple expression `[x,y]`), so language designers may use this syntax for function calls if desired.
1. Custom infix operations with precedence given by user-specified precedence table.
1. Parentheses are *only* for the purpose of clarifying precedence, so specifically parentheses are always idempotent
1. Built-in support for sum and product type syntax:

    - Product type with field names: `{ a = 2; b = "hello"; }`. This is also the block/let syntax, because `{ a = 2; a + a }` is sugar for `{ a = 2; ret = a + a; }.ret` (where `ret` is a fresh name)
    - Product type without field names: `[2, "hello"]`. We don't use parentheses for tuples because that would violate the rule that parentheses are only for clarifying precedence
    - Field access: `x .a`. Note that fields are first-class syntax objects, `x.a` parses as the application of `x` to `.a`. This makes sense because we think of a record `{ a = 1; b = 2; }` as something like a function out of the set `{ .a, .b }` (note that `{ .a, .b }` is not a valid f-expr because expressions in curly braces must be separated by semicolons).
    - Construction of elements of a sum type: `'left 3`. The use of `'` for sum type tags is inspired by the lisp tradition of `'x` for symbols, but also because `'` is "typographically dual" to `.`.

1. Support a no keywords style via:

    - Primitives: `@u32.add[1, 2]` for exposing fundamental operations (things that evaluate all of their arguments like a normal function does)
    - Specials: `%if[x >= 0, 'nonnegative, 'negative]` for things that could do *anything*, including looking at the syntax of their argument values

    Thus, in a f-expression based language, one can always use any variable name without fear of clashing with a builtin keyword. Of course, there may be a standard library which re-exposes primitives/specials as normal functions, but this standard library is not privileged from the compiler perspective.
1. Application with no intervening space binds tighter than application with space, so `f x.1 x.2` parses as `f (x .1) (x .2)` instead of `f x .1 x .2`, and `'some @alloc[1,2]` parses as `'some (@alloc [1, 2])` instead of `'some @alloc [1,2]`.

## Inspirations

- lisp
- [Julia](https://julialang.org)
- [narya](https://github.com/mikeshulman/narya/)
- [rhombus](https://docs.racket-lang.org/rhombus/index.html)
