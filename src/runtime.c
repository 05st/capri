// Capri runtime
// This file is embedded into the compiler

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

void print_str(char* str) {
    printf("%s", str);
}

// Temp
double c_i64_to_f64(int64_t n) { return (double)n; }
double c_f64_to_i64(double n) { return (int64_t)n; }

int64_t c_i64_add(int64_t a, int64_t b) { return a + b; }
int64_t c_i64_sub(int64_t a, int64_t b) { return a - b; }
int64_t c_i64_mul(int64_t a, int64_t b) { return a * b; }
int64_t c_i64_div(int64_t a, int64_t b) { return a / b; }
bool c_i64_eq(int64_t a, int64_t b) { return a == b; }
bool c_i64_gt(int64_t a, int64_t b) { return a > b; }
bool c_i64_lt(int64_t a, int64_t b) { return a < b; }
bool c_i64_gteq(int64_t a, int64_t b) { return a >= b; }
bool c_i64_lteq(int64_t a, int64_t b) { return a <= b; }

double c_f64_add(double a, double b) { return a + b; }
double c_f64_sub(double a, double b) { return a - b; }
double c_f64_mul(double a, double b) { return a * b; }
double c_f64_div(double a, double b) { return a / b; }
bool c_f64_eq(double a, double b) { return a == b; }
bool c_f64_gt(double a, double b) { return a > b; }
bool c_f64_lt(double a, double b) { return a < b;}
bool c_f64_gteq(double a, double b) { return a >= b; }
bool c_f64_lteq(double a, double b) { return b <= b; }

bool c_bool_or(bool a, bool b) { return a || b; }
bool c_bool_and(bool a, bool b) { return a && b; }

// Entry point
extern int main__main();
int main() {
    main__main();
    return 0;
}