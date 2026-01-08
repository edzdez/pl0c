module Elab = Elab
module Typecheck = Typecheck

let semant block = block |> Elab.elab |> Typecheck.typecheck
