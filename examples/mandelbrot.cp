module main;

extern putchar(i32): unit;
extern printf(str, i32): unit;
extern clock(): i32;

fn pdensity(d: i32)
    if d > 8
        putchar(32)
    else if d > 4
        putchar(46)
    else if d > 2
        putchar(43)
    else
        putchar(42);

fn converge(cr, ci) {
    mut r := 0.0;
    mut i := 0.0;
    mut r2 := 0.0;
    mut i2 := 0.0;
    mut iters := 0;
    while (r2 + i2 < 4) && (iters < 256) {
        i = r * i * 2 + ci;
        r = r2 - i2 + cr;
        r2 = r * r;
        i2 = i * i;
        iters = iters + 1;
    };
    iters
};

fn plotHelper(xmin, xmax, xstep, ymin, ymax, ystep) {
    mut y := ymin;
    while y < ymax {
        mut x := xmin;
        while x < xmax {
            pdensity(converge(x, y));
            x = x + xstep;
        };
        putchar(10);
        y = y + ystep;
    };
};

fn main() {
    rs := 0.0-2.5;
    is := 0.0-1.3;
    rm := 0.05;
    im := 0.07;

    t: i32 := clock();
    plotHelper(rs, rs + (rm*(f64)78), rm, is, is + (im*(f64)40), im);
    printf("Mandelbrot Render (clock ticks): %d\n", clock() - t);
};