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
- The IR and MIR are _too_ low-level right now, and are thus very hard to work with.
  In particular, it is _basically_ already assembly, except that we allow an unlimited
  number of virtual registers (in fact, I even insert function prologues and epilogues!).
  This significantly complicates instruction selection, as well as register allocation,
  since it's now very difficult to generate spill code.
  In the future, I'd instead:
  - Add type info to the IR, so that I can figure out what kind of register is needed to
    store a particular value. Right now, since we only support 32-bit integers in the source
    language, I'm able to just hard-code it.
  - To handle stack accesses, I can try to do something similar to what (I believe) LLVM
    does: use `alloca` instructions to get `ptr`s to stack locations to be resolved later.
  - The MIR should still be a pseudo-assembly similar to the IR, but containing additional
    information:
    - map type info to sizes,
    - track liveness information (def, use, **clobber** <- IMPORTANT!)
    - DO NOT EMIT PROLOGUES AND EPILOGUES YET! ONLY AFTER REGALLOC, WHEN WE'VE DETERMINED
      WHAT TO SPILL!
  - Emitting actual assembly should be saved for the **VERY END**.
