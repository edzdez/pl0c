module East = East
module Elab = Elab
module Typecheck = Typecheck

let semant block = block |> Elab.elab |> Typecheck.typecheck
let to_string = East.to_string
