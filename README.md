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

## Installation
### Compiling from source
1. Ensure [Stack](https://docs.haskellstack.org/en/stable/README/) and [Git](https://git-scm.com/) are installed. *\**
2. Clone this repository (`git clone https://github.com/05st/capri.git`).
3. Run `stack build` in the cloned repository.
4. The resulting binary can be found under `./.stack-work/dist/<something>/build/capri-exe/`.

*\* It's recommended to install Stack via [ghcup](https://www.haskell.org/ghcup/).*

### Downloading precompiled binaries
Check the [releases](https://github.com/05st/capri/releases) page for the latest precompiled binaries.

## Documentation
Language documentation can be found on the [wiki](https://github.com/05st/capri/wiki).
