# Unnecessarily complex fibonacci example
## The following is an example showing some of the language's syntax. It asks the user for input "n" and prints n fibonacci numbers in order.
### The `scan()` and `print(x)` functions are defined in an external library. They are wrappers for `scanf` and `printf`

```rust
dec actn print(x);
dec scan();

fn fibrec(n, m, prt) 
{
    if (n > m) * prt
    {
        print(fibrec(n-1, m, 1));
    };

    if n < 2 
    {
        ret 1;
    };

    ret fibrec(n-1, m, 0) + fibrec(n-2, m, 0);
};

actn fib(n) 
{
    print(fibrec(n, 1, 1));
};

actn main()
{
    fib(scan());
};
```
## Here's an example using loops.

```rust
dec actn print(x);
dec scan();

fn fib(n) 
{
    let a := 1;
    let b := 1;
    let c := 0;

    let i := 3;
    loop 
    {
        c := a + b;
        a := b;
        b := c;

        i := i + 1;
        if i > (n + 1) { break; };
    };

    ret b;
};

actn main() 
{
    let max := scan();	
    print(1);
    let i := 2;
    loop
    {
        print(fib(i));
        i := i + 1;
        if i > max { break; };
    };
};
```
