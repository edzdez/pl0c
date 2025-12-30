open! Core
module P = Parse.Parser
module L = Parse.Lexer
module E = Semant.Elab
module Mark = Util.Mark

let elab lexbuf =
  match P.program L.read lexbuf with
  | Some ast ->
    (try Some (E.elab ast) with
     | E.Elab_error { data; span } ->
       printf
         "[%s]: %s\n"
         (Mark.to_string span)
         (E.sexp_of_elab_error data |> Sexp.to_string);
       None)
  | None -> assert false
;;

let%expect_test "properly elaborates the minimal program" =
  In_channel.with_file "../examples/parse_minimal.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Semant.to_string ast);
      [%expect {| begin end. |}])
;;

let%expect_test "reports an error on undeclared variable usage" =
  In_channel.with_file "../examples/semant_undeclaredvariables.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> [%expect {| [2:2-2:3]: (Undeclared_ident x) |}]
    | Some _ -> assert false)
;;

let%expect_test "reports an error on undeclared procedure usage" =
  In_channel.with_file "../examples/semant_undeclaredprocedure.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> [%expect {| [2:7-2:8]: (Undeclared_ident p) |}]
    | Some _ -> assert false)
;;

let%expect_test "reports an error when assigning to constants" =
  In_channel.with_file "../examples/semant_assigntoconstant.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> [%expect {| [3:2-3:3]: (Not_lvalue a) |}]
    | Some _ -> assert false)
;;

let%expect_test "reports an error when assigning to procedure" =
  In_channel.with_file "../examples/semant_assigntoprocedure.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> [%expect {| [4:2-4:3]: (Not_lvalue p) |}]
    | Some _ -> assert false)
;;

let%expect_test "reports an error when calling a constant" =
  In_channel.with_file "../examples/semant_callconstant.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> [%expect {| [3:7-3:8]: (Not_procedure x) |}]
    | Some _ -> assert false)
;;

let%expect_test "reports an error when calling a variable" =
  In_channel.with_file "../examples/semant_callvariable.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> [%expect {| [3:7-3:8]: (Not_procedure x) |}]
    | Some _ -> assert false)
;;

let%expect_test "reports an error on procedure call before definition" =
  In_channel.with_file "../examples/semant_procedureorder.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> [%expect {| [3:7-3:8]: (Undeclared_ident A) |}]
    | Some _ -> assert false)
;;

let%expect_test "properly handles procedure scoping" =
  In_channel.with_file "../examples/semant_procedurescoping.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> [%expect {| [9:7-9:8]: (Undeclared_ident A) |}]
    | Some _ -> assert false)
;;

let%expect_test "properly supports variable shadowing" =
  In_channel.with_file "../examples/semant_shadowing.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Semant.to_string ast);
      [%expect
        {|
        const sym$7 = 0;
        procedure sym$8;
        const sym$9 = 1;
        begin
          !(sym$9)
        end;
        begin
          !(sym$7)
        end.
        |}])
;;

let%expect_test "properly handles variable scoping" =
  In_channel.with_file "../examples/semant_variablescoping.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> [%expect {| [8:2-8:3]: (Undeclared_ident x) |}]
    | Some _ -> assert false)
;;

let%expect_test "properly elaborates a complex program" =
  In_channel.with_file "../examples/fibonacci.pl0" ~f:(fun fin ->
    let lexbuf = Lexing.from_channel fin in
    let ast = elab lexbuf in
    match ast with
    | None -> assert false
    | Some ast ->
      printf "%s\n" (Semant.to_string ast);
      [%expect
        {|
        var sym$12, sym$13, sym$14, sym$15;
        begin
          sym$12 := 0;
          sym$13 := 1;
          ?sym$15;
          while (sym$15) > (0) do
            begin
              !(sym$12);
              sym$14 := (sym$12) + (sym$13);
              sym$12 := sym$13;
              sym$13 := sym$14;
              sym$15 := (sym$15) - (1)
            end
        end.
        |}])
;;
