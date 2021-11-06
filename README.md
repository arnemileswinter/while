# While

This is an interpreter for the WHILE programming language.

See [Releases](https://github.com/arnemileswinter/while/releases) to get the latest version.

## Language

The WHILE programming language features a limited set of operations.
Namely these are:

### Concatenation

WHILE programs are concatenated via semicolon.

### Variable assignment

Variables in WHILE must start with an X and are followed by a natural integer.

```
x1 = 1;
x2 = 2;
x3 = 3
```

running this using `./do-while` will yield:

```
| Register | Value |
|----------|-------|
| 1        | 1     |
| 2        | 2     |
| 3        | 3     |
```

### Addition and Subtraction

WHILE programs support the following operations:

```
x1 = 1;
x2 = 2 + 3;
x3 = x1 + 1;
x4 = x2 + x3
```

running this using `./do-while` will yield:

```
| Register | Value |
|----------|-------|
| 1        | 1     |
| 2        | 5     |
| 3        | 2     |
| 4        | 7     |
```

Subtraction is also supported via the infix `-` operand. 

Note that there is no unary `-`. Negative numbers are supported by subtraction of `0` with a constant, e.g. `x1 = 0 - 100`.

### FOR statement

FOR statements take the current value of the variable and repeat the loop-body accordingly.

```
x1 = 0;
x2 = 5;
for x2 do
   x1 = x1 + 1
end
```

will yield:

```
| Register | Value |
|----------|-------|
| 1        | 5     |
| 2        | 5     |
```

### WHILE statement

WHILE statements repeat their body until your variable reaches 0. Note that WHILE statements may be non-terminating.
The only implemented predicate is to repeat until 0.

```
x1 = 1;
x2 = 0;
while x1 != 0 do
    x2 = 1;
    x1 = 0
end
```

will yield:

```
| Register | Value |
|----------|-------|
| 1        | 0     |
| 2        | 1     |
```

As a matter of fact, the above code produces the same functionality as an IF-statement in other programming languages.

## Tooling

Included are the programs `do-while` and `pretty-while`.

## do-while

`do-while` will interpret your WHILE programs.
If the `--input-file myfile.while` is provided, program contents are read from there. Otherwise `do-while` will interpret the program written to `stdin`.

## pretty-while

`pretty-while` is a code formatter for WHILE programs.
Running `pretty-while` without arguments will prettify the program written to `stdin`. 
Supplying `--input-file myfile.while` formats myfile.while accordingly.
If the `--write-file` flag is provided, the file contents are replaced in the input file, rather than printed to stdout.

