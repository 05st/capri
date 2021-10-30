module io;

extern printf(str, str): unit;
extern scanf(str, char*): unit;
extern malloc(i32): i32;

pub fn print(string: str): unit
    printf("%s", string);

pub fn println(string: str): unit {
    printf("%s\n", string);
};

pub fn input(): str {
    buffer := (char*)malloc(256);
    scanf("%s", buffer);
    (str)buffer
};
