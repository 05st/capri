// Capri runtime
// This file is embedded into the compiler

#include <stdio.h>
#include <stdlib.h>

void print_str(char* str) {
    printf("%s", str);
}

// Entry point
extern int main__main();
int main() {
    main__main();
    return 0;
}