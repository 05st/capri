# Juno
A statically typed, functional programming language which compiles to C.
Takes inspiration from languages such as Rust or Haskell.

![Total Lines](https://img.shields.io/tokei/lines/github/05st/juno)

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
- Parametric polymorphism
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
More examples [here](https://github.com/05st/juno/tree/master/examples)

## Installation
Installation steps can be found [here](https://github.com/05st/juno/wiki/Installation)

## Usage
- To compile a file, simply run `juno path/to/file.jn --stl path/to/stl/`
- For directories, turn on the `-d` (`--dir`) flag: `juno -d path/to/dir/ --stl path/to/stl/`
- To change the output path, use `-o` (`--out`): `juno path/to/file.jn -o path/to/out.c --stl path/to/stl/` \
\
Run `juno -h` for more help.

## Documentation
Visit the [wiki](https://github.com/05st/juno/wiki) for documentation on the language.
