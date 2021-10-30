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
