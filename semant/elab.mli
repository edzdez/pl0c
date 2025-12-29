(**
 * Semantic Elaboration.
 *
 * In this module, we
 *  1. construct a symbol table where each declaration is assigned a unique symbol id and
 *     given some semantic information (kind, type (eventually) owning procedure, etc.).
 *  2. rewrite the AST so that each reference to a logical identifier is replaced with a
 *     reference to the correct symbol id.
 *
 * This step emits errors for:
 *  - Undeclared identifiers
 *  - Duplicate identifiers
 *)
