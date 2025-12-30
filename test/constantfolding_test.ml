open! Core
module P = Parse.Parser
module L = Parse.Lexer
module C = Opt.Constants
module S = Semant
module Mark = Util.Mark

let opt lexbuf =
  match P.program L.read lexbuf with
  | None -> assert false
  | Some ast ->
    let ast = S.semant ast in
    (try Some (C.fold ast) with
     | C.Constant_folding_error { data; span } ->
       printf
         "[%s]: %s\n"
         (Mark.to_string span)
         (C.sexp_of_constant_folding_error data |> Sexp.to_string);
       None)
;;

let%expect_test "the minimal program passes through" =
  In_channel.with_file "../examples/parse_minimal.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = opt lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Semant.to_string ast);
      [%expect {| begin end. |}])
;;

let%expect_test "simple constant folding works" =
  In_channel.with_file "../examples/opt_constantfolding1.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = opt lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Semant.to_string ast);
      [%expect
        {|
        begin
          !(14)
        end.
        |}])
;;

let%expect_test "simple constant folding with constant names works" =
  In_channel.with_file "../examples/opt_constantfolding2.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = opt lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Semant.to_string ast);
      [%expect
        {|
        begin
          !(14)
        end.
        |}])
;;

let%expect_test "does not rewrite subexpressions with variables" =
  In_channel.with_file "../examples/opt_constantfolding3.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = opt lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Semant.to_string ast);
      [%expect
        {|
        var sym$3;
        begin
          !((2) + ((4) * (sym$3)))
        end.
        |}])
;;

let%expect_test "reports an error on an obvious divide by zero" =
  In_channel.with_file "../examples/opt_constantfolding_divbyzero.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = opt lexbuf in
    match ast with
    | None -> [%expect {| [2:3-2:10]: Division_by_zero |}]
    | Some _ -> assert false)
;;

let%expect_test "reports an error on a divide by zero with constant names" =
  In_channel.with_file "../examples/opt_constantfolding_divbyzero1.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = opt lexbuf in
    match ast with
    | None -> [%expect {| [3:3-3:16]: Division_by_zero |}]
    | Some _ -> assert false)
;;

let%expect_test "cannot catch divide by zeros with variables" =
  In_channel.with_file "../examples/opt_constantfolding_divbyzero2.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = opt lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Semant.to_string ast);
      [%expect
        {|
        var sym$6;
        begin
          sym$6 := 5;
          !((5) / ((sym$6) - (5)))
        end.
        |}])
;;

let%expect_test "catches overflow" =
  In_channel.with_file "../examples/opt_constantfolding_overflow.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = opt lexbuf in
    match ast with
    | None -> [%expect {| [3:3-3:10]: Overflow |}]
    | Some _ -> assert false)
;;

let%expect_test "complex constant folding" =
  In_channel.with_file "../examples/opt_constantfolding.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = opt lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Semant.to_string ast);
      [%expect
        {|
        procedure sym$11;
        begin end;
        begin
          call sym$11;
          !(sym$10)
        end.
        |}])
;;
