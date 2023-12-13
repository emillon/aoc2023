open Base
open Lib
open Stdio

let sample =
  [ "Time:      7  15   30"; "Distance:  9  40  200" ] |> String.concat_lines

type race = { time : int; distance : int } [@@deriving sexp]
type t = race list [@@deriving sexp]

let parse =
  let open Angstrom in
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
  parse input

let%expect_test "parser" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((time 7) (distance 9)) ((time 15) (distance 40))
     ((time 30) (distance 200))) |}]

let result_for_race { time; distance } =
  let r = ref 0 in
  for hold_time = 0 to time do
    let speed = hold_time in
    let remaining_time = time - hold_time in
    let score = speed * remaining_time in
    if score > distance then Int.incr r
  done;
  !r

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

let%expect_test "result" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 71503 |}]

let run () = main All parse result result2
