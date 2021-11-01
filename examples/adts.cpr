module main;

extern printf(str, i32): unit;

type IntOrBool = Int(i32) | Bool(bool);

fn main() {
    xyz := Int(420);
    test := match xyz {
        Bool(_) => 0,
        Int(n) => n
    };
    printf("%d\n", test);
};
