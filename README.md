# Juno
A statically typed, functional programming language which compiles to C.
Takes inspiration from languages such as Rust or Haskell.

![Total Lines](https://img.shields.io/tokei/lines/github/05st/juno)

## Example
```
extern printf(str, i32): unit;
extern scanf(str, int*): unit;

fn factorial(n)
  if n <= 1
    1
  else
    n * factorial(n - 1);

fn main() {
  mut n := 0;
  scanf("%d", &n);
  printf("%d\n", factorial(n));
};
```
More examples [here](https://github.com/05st/juno/tree/master/examples)
