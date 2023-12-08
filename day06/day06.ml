open Base
open Lib
open Stdio

let sample =
  [ "Time:      7  15   30"; "Distance:  9  40  200" ] |> String.concat_lines

type race = { time : int; distance : int } [@@deriving sexp]
type t = race list [@@deriving sexp]

let parse s =
  let open Angstrom in
  let open Angstrom_helpers in
  let numbers =
    take_while1 Char.is_whitespace
    *> Angstrom.sep_by1 (Angstrom.take_while1 Char.is_whitespace) number
    <* end_of_line
  in
  let input =
    let+ time_line = string "Time:" *> numbers
    and+ distance_line = string "Distance:" *> numbers in
    List.zip_exn time_line distance_line
    |> List.map ~f:(fun (time, distance) -> { time; distance })
  in
  Angstrom.parse_string ~consume:All input s |> Result.ok_or_failwith

let%expect_test "parser" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((time 7) (distance 9)) ((time 15) (distance 40))
     ((time 30) (distance 200))) |}]

let runs { time; distance } =
  List.range 0 time ~stop:`inclusive
  |> List.map ~f:(fun hold_time ->
         let speed = hold_time in
         let remaining_time = time - hold_time in
         let score = speed * remaining_time in
         let beats_record = score > distance in
         (hold_time, score, beats_record))

let%expect_test "runs" =
  let test r = runs r |> [%sexp_of: (int * int * bool) list] |> print_s in
  test { time = 7; distance = 9 };
  [%expect
    {|
    ((0 0 false) (1 6 false) (2 10 true) (3 12 true) (4 12 true) (5 10 true)
     (6 6 false) (7 0 false)) |}];
  test { time = 15; distance = 40 };
  [%expect
    {|
    ((0 0 false) (1 14 false) (2 26 false) (3 36 false) (4 44 true) (5 50 true)
     (6 54 true) (7 56 true) (8 56 true) (9 54 true) (10 50 true) (11 44 true)
     (12 36 false) (13 26 false) (14 14 false) (15 0 false)) |}]

let result_for_race r = runs r |> List.count ~f:(fun (_, _, ok) -> ok)
let result l = List.map l ~f:result_for_race |> product

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 288 |}]

let combine_inputs l =
  let time_s, distance_s =
    List.fold l ~init:("", "")
      ~f:(fun (acc_times, acc_distances) { time; distance } ->
        (acc_times ^ Int.to_string time, acc_distances ^ Int.to_string distance))
  in
  { time = Int.of_string time_s; distance = Int.of_string distance_s }

let result2 l = combine_inputs l |> result_for_race
let%expect_test "result" = parse sample |> result2 |> printf "%d\n";
  [%expect {| 71503 |}]

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_all path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_all path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
