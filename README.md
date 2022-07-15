# Capri
Capri is a statically typed, general purpose programming language with an emphasis on functional programming and simplicity. It takes inspiration from other languages such as Rust or Haskell.

![Total Lines](https://img.shields.io/tokei/lines/github/05st/capri)
![Latest Release](https://img.shields.io/github/v/release/05st/capri?include_prereleases)

The compiler currently targets only C. Everything in the language is subject to change. Major planned features such as polymorphism are also currently missing. Here is a list of so far implemented features:
- Type inference with an extended Hindley-Milner type system
- Records and variants (algebraic data types)
- Module system (which will allow some sort of dynamic linking)
- User-defined prefix, infix, and postfix operators with arbitrary precedences
- Pattern matching (exhaustiveness checking is planned)
- A small standard library

Planned features:
- Parametric polymorphism
- Row polymorphism
- Ad-hoc polymorphism (most likely via typeclasses)
- Match expression exhaustiveness checking
- Possibly some sort of memory management

Examples can be found [here](https://github.com/05st/capri/tree/master/examples).

## Documentation, Installation/Usage Instructions
Language documentation and instructions can be found on the [docs](https://05st.github.io/capri-website/).
