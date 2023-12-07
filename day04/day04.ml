open Base
open Lib
open Stdio

let sample =
  [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53";
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19";
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83";
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36";
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
  ]

type line = { winning : int list; have : int list } [@@deriving sexp]
type t = line list [@@deriving sexp]

let parse_line s =
  let open Angstrom_helpers in
  let numbers =
    let open Angstrom in
    sep_by1 (take_while1 Char.is_whitespace) number
  in
  let line =
    let open Angstrom in
    let+ winning =
      string "Card"
      *> take_while1 Char.is_whitespace
      *> number *> string ":"
      *> take_while1 Char.is_whitespace
      *> numbers
    and+ have = string " |" *> take_while1 Char.is_whitespace *> numbers in
    { winning; have }
  in
  Angstrom.parse_string ~consume:All line s |> Result.ok_or_failwith

let parse lines = List.map ~f:parse_line lines

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((winning (41 48 83 86 17)) (have (83 86 6 31 17 9 48 53)))
     ((winning (13 32 20 16 61)) (have (61 30 68 82 17 32 24 19)))
     ((winning (1 21 53 59 44)) (have (69 82 63 72 16 21 14 1)))
     ((winning (41 92 73 84 69)) (have (59 84 76 51 58 5 54 83)))
     ((winning (87 83 26 28 32)) (have (88 30 70 12 93 22 82 36)))
     ((winning (31 18 13 56 72)) (have (74 77 10 23 35 67 36 11)))) |}]

let count line =
  List.count line.have ~f:(fun n -> List.mem line.winning n ~equal:Int.equal)

let score line =
  let count = count line in
  if count = 0 then 0 else Int.pow 2 (count - 1)

let result lines = List.map lines ~f:score |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 13 |}]

let result2 lines =
  let cards = Array.of_list_map lines ~f:(fun _ -> 1) in
  let counts = Array.of_list_map lines ~f:count in
  Array.iteri counts ~f:(fun i count ->
      for j = i + 1 to i + count do
        cards.(j) <- cards.(j) + cards.(i)
      done);
  Array.fold cards ~f:( + ) ~init:0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 30 |}]

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_lines path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_lines path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
