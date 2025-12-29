(**
 * Typechecking.
 *
 * PL/0 only has one type: integers.
 * Typechecking is thus very simple, and hence perhaps doesn't warrant its own module.
 * We decided, however, to create this module regardless since:
 *  1. we hope to add more types to the language in the future, and
 *  2. we still do need somewhere to check kinds, e.g. we only call procedures, only
 *     assign to variables, etc.
 *
 * This step emits errors for:
 *  - Invalid kinds
 *)
