open Base
open Stdio

let sample = [ "1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet" ]

let extract_all_digits s =
  String.to_list s
  |> List.filter_map ~f:(fun c ->
         if Char.is_digit c then Some (Char.to_int c - Char.to_int '0')
         else None)

let%expect_test "extract_all_digits" =
  let test s = extract_all_digits s |> [%sexp_of: int list] |> print_s in
  test "1abc2";
  [%expect {| (1 2) |}];
  test "a1b2c3d4e5f";
  [%expect {| (1 2 3 4 5) |}]

let first_and_last l = (List.hd_exn l, List.last_exn l)
let sum l = List.fold ~f:( + ) ~init:0 l
let make_number (a, b) = (10 * a) + b

let result l =
  List.map l ~f:(fun line ->
      extract_all_digits line |> first_and_last |> make_number)
  |> sum

let%expect_test "result" =
  printf "%d\n" (result sample);
  [%expect {| 142 |}]

let run () =
  match Sys.get_argv () with
  | [| _; path |] -> In_channel.read_lines path |> result |> printf "%d\n"
  | _ -> assert false
