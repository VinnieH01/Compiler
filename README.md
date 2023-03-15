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
