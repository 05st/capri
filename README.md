# Capri
A statically typed, functional programming language which compiles to C.
Takes inspiration from languages such as Rust or Haskell.

![Total Lines](https://img.shields.io/tokei/lines/github/05st/capri)
![Latest Release](https://img.shields.io/github/v/release/05st/capri?include_prereleases)

## Features
- Type inference (no type annotations required)
- Algebraic data types
- Recursively defined types
- User definable prefix, infix, and postfix operators
- Pattern matching
- Nearly everything is an expression
- Immutable variables by default
- Concise syntax
- Module system

## Planned
- Parametric polymorphism (currently only functions)
- Typeclasses

## Example
```
module main;

extern printf(str, i32): unit;
extern scanf(str, i32*): unit;

// Factorial
op postfix 10 ! (n)
  if n <= 1
    1
  else
    n * (n - 1)!;

fn main() {
  mut n := 0;
  scanf("%d", &n);
  printf("%d\n", n!);
};
```
More examples [here](https://github.com/05st/capri/tree/master/examples).

## Installation
Installation steps can be found [here](https://github.com/05st/capri/wiki/Installation).

## Usage
Usage instructions are found [here](https://github.com/05st/capri/wiki/Usage).

## Documentation
Visit the [wiki](https://github.com/05st/capri/wiki) for documentation on the language.
