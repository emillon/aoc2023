open Base
open Lib
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

let make_number (a, b) =
  assert (a < 10);
  assert (a > 0);
  assert (b < 10);
  assert (b > 0);
  (10 * a) + b

let result l =
  List.map l ~f:(fun line ->
      extract_all_digits line |> first_and_last |> make_number)
  |> sum

let%expect_test "result" =
  printf "%d\n" (result sample);
  [%expect {| 142 |}]

let sample2 =
  [
    "two1nine";
    "eightwothree";
    "abcone2threexyz";
    "xtwone3four";
    "4nineeightseven2";
    "zoneight234";
    "7pqrstsixteen";
  ]

let part2_str_re =
  Str.regexp
    "[123456789]\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine"

let extract_all_digits_2 s =
  let value = function
    | "1" | "one" -> 1
    | "2" | "two" -> 2
    | "3" | "three" -> 3
    | "4" | "four" -> 4
    | "5" | "five" -> 5
    | "6" | "six" -> 6
    | "7" | "seven" -> 7
    | "8" | "eight" -> 8
    | "9" | "nine" -> 9
    | s -> raise_s [%message "extract_all_digits2" (s : string)]
  in
  let _ = Str.search_forward part2_str_re s 0 in
  let first = value (Str.matched_string s) in
  let _ = Str.search_backward part2_str_re s (String.length s - 1) in
  let last = value (Str.matched_string s) in
  (first, last)

let%expect_test "extract_all_digits_2" =
  List.map sample2 ~f:(fun s -> (s, extract_all_digits_2 s))
  |> [%sexp_of: (string * (int * int)) list] |> print_s;
  [%expect
    {|
    ((two1nine (2 9)) (eightwothree (8 3)) (abcone2threexyz (1 3))
     (xtwone3four (2 4)) (4nineeightseven2 (4 2)) (zoneight234 (1 4))
     (7pqrstsixteen (7 6))) |}]

let result2 l =
  List.map l ~f:(fun line -> extract_all_digits_2 line |> make_number) |> sum

let%expect_test "result2" =
  result2 sample2 |> printf "%d\n";
  [%expect {| 281 |}]

let run () =
  match Sys.get_argv () with
  | [| _; path |] -> In_channel.read_lines path |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_lines path |> result2 |> printf "%d\n"
  | _ -> assert false
