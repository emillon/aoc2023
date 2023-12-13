open Base
open! Lib
open Stdio

let sample = [ "0 3 6 9 12 15"; "1 3 6 10 15 21"; "10 13 16 21 30 45" ]

type t = int list list [@@deriving sexp]

let parse_line =
  let open Angstrom in
  let signed_number =
    let+ minus = take_while (Char.equal '-') and+ number in
    match minus with "-" -> -number | "" -> number | _ -> assert false
  in
  let line = sep_by1 (char ' ') signed_number in
  parse line

let parse = List.map ~f:parse_line

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect {| ((0 3 6 9 12 15) (1 3 6 10 15 21) (10 13 16 21 30 45)) |}]

let rec diffs = function
  | a :: (b :: _ as l) -> (b - a) :: diffs l
  | [ _ ] -> []
  | [] -> assert false

let%expect_test "diffs" =
  diffs [ 10; 13; 16; 21; 30; 45 ] |> [%sexp_of: int list] |> print_s;
  [%expect {| (3 3 5 9 15) |}]

let is_all_zeroes = List.for_all ~f:(fun x -> x = 0)

let all_diffs =
  unfold ~f:(fun l ->
      if is_all_zeroes l then None
      else
        let ds = diffs l in
        Some (l, ds))

let%expect_test "all_diffs" =
  all_diffs [ 10; 13; 16; 21; 30; 45 ] |> [%sexp_of: int list list] |> print_s;
  [%expect {| ((10 13 16 21 30 45) (3 3 5 9 15) (0 2 4 6) (2 2 2)) |}]

let result_one l = all_diffs l |> List.map ~f:List.last_exn |> sum

let%expect_test "result_one" =
  [ 10; 13; 16; 21; 30; 45 ] |> result_one |> printf "%d\n";
  [%expect {| 68 |}]

let result ll = List.map ll ~f:result_one |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 114 |}]

let result2 ll = List.map ~f:List.rev ll |> result

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 2 |}]

let run () = main Lines parse result result2
