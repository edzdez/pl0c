(**
 * Constant Folding.
 *
 * This module evaluates constant expressions at compile time.
 * In particular, we inline references to constant variables and fold arithmetic
 * expressions where all arguments are proven constants, i.e. we don't ever fold vars.
 *)
