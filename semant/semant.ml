module Elab = Elab
module Typecheck = Typecheck
module Constants = Constants

let semant block = block |> Elab.elab |> Typecheck.typecheck |> Constants.fold
let to_string = East.to_string
