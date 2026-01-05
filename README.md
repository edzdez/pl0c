# PL/0c

A compiler for the [PL/0](https://en.wikipedia.org/wiki/PL/0) programming language written in OCaml.

The language has been extended to support the following features:

- Comments of the form `{ ... }`.
- Input and output, respectively handled by operators `?` and `!`.

In the future, we hope to support some additional features:

- Floating-point numbers.
- More logical operators.
- Strings.
- `else` and `else if`
- Arrays.
- Procedure arguments.

## Lessons Learned

- In the IR, it's a good idea to distinguish between variables and temporaries, so that
  we can more easily avoid a bunch of redundant load-stores.
- I should store a procedure's Temp generators (e.g. for virtual registers) in the symbol
  table; right now, we lose that information, and so we can't really generate additional
  virtual registers in later stages.
- I think that both the IR and the MIR might be a bit _too_ low-level right now.
  It makes it really hard to work with.
