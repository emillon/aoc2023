open Base
open! Lib
open Stdio

let sample =
  [ "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" ]
  |> String.concat_lines

let parse s = s |> String.rstrip |> String.split ~on:','

type t = string list [@@deriving sexp]

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect {| (rn=1 cm- qp=3 cm=2 qp- pc=4 ot=9 ab=5 pc- pc=6 ot=7) |}]

let eval s =
  String.fold s ~init:0 ~f:(fun acc c -> 17 * (acc + Char.to_int c) % 256)

let%expect_test "eval" = eval "HASH" |> [%sexp_of: int] |> print_s;
  [%expect {| 52 |}]
let result l = l |> List.map ~f:eval |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 1320 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
