# Hexadecimal

Write a program that will convert a hexadecimal number, represented as a string (e.g. "10af8c"), to its decimal equivalent using first principles (i.e. no, you may not use built-in or external libraries to accomplish the conversion).

On the web we use hexadecimal to represent colors, e.g. green: 008000,
teal: 008080, navy: 000080).

The program should handle invalid hexadecimal strings.

For running the tests provided, you will need `Opam` and `Core`. Consult [opam](https://opam.ocaml.org) website for instuctions on how to install for your OS. Once `opam` is installed open a terminal window and run the following command to install core:

opam install core

## Running Tests

Because OCaml is a compiled language you need to compile your submission and the test code before you can run the tests. Compile with

```bash
$ corebuild -quiet test.native
```

and when successful run the tests by running the `test.native` executable:

```bash
./test.native
```

Alternatively just type

```bash
make
```

## Source

All of Computer Science [view source](http://www.wolframalpha.com/examples/NumberBases.html)
