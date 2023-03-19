# Mandelbrot set example
## The following is an example showing some of the language's syntax. It displays the mandelbrot set using ascii characters
### The `printchar(i8)` function is defined in an external library `lib.c`. The code is a translation of the one found in the LLVM Kaleidoscope tutorial.

```rust
dec actn printchar(i8: char);

actn printdensity(i32: d)
{
    if d > i32: 8
    {
        printchar(i8: 32);
    } 
    else if d > i32: 4
    {
        printchar(i8: 46);
    } 
    else if d > i32: 2 
    {
        printchar(i8: 43);
    }
    else
    {
        printchar(i8: 42);
    };
};

fn mandleconverger(f64: real, f64: imag, i32: iters, f64: creal, f64: cimag) -> i32
{
    if iters > i32: 255 | (real * real + imag * imag > f64: 4)
    {
        ret iters;
    };
    ret mandleconverger(real * real - imag * imag + creal, 
                        f64: 2 * real * imag + cimag, iters + i32: 1, creal, cimag);
};

fn mandleconverge(f64: real, f64: imag) -> i32
{
    ret mandleconverger(real, imag, i32: 0, real, imag);
};

actn mandelhelp(f64: xmin, f64: xmax, f64: xstep, f64: ymin, f64: ymax, f64: ystep)
{
    let f64: y <- ymin;
    while y < ymax
    {
        let f64: x <- xmin;
        while x < xmax
        {
            printdensity(mandleconverge(x, y));
            x <- x + xstep;
        };
        printchar(i8: 10);
        y <- y + ystep;
    };
};

actn mandel(f64: realstart, f64: imagstart, f64: realmag, f64: imagmag) 
{
    mandelhelp(realstart, realstart + realmag * f64: 78, realmag, 
               imagstart, imagstart + imagmag * f64: 40, imagmag);
};

actn main() 
{
    mandel(-f64: 2.3, -f64: 1.3, f64: 0.05, f64: 0.07);
};
```
