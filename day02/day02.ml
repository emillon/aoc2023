open Base
open Stdio

let sample =
  [
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red";
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";
  ]

type subset = { red : int; green : int; blue : int } [@@deriving sexp]

let subset_is_correct { red; green; blue } =
  red <= 12 && green <= 13 && blue <= 14

type line = { id : int; subsets : subset list } [@@deriving sexp]

let subset_from_components components =
  let empty_subset = { red = 0; green = 0; blue = 0 } in
  List.fold components ~init:empty_subset ~f:(fun acc (color, amount) ->
      match color with
      | `Red -> { acc with red = amount }
      | `Green -> { acc with green = amount }
      | `Blue -> { acc with blue = amount })

let parse_line s =
  let number =
    let open Angstrom in
    (let* s = take_while1 Char.is_digit in
     return (Int.of_string s))
    <?> "number"
  in
  let id = number in
  let component =
    let open Angstrom in
    (let* amount = number in
     let* _ = string " " in
     let* color =
       choice
         [
           (let+ _ = string "red" in
            `Red);
           (let+ _ = string "green" in
            `Green);
           (let+ _ = string "blue" in
            `Blue);
         ]
     in
     return (color, amount))
    <?> "component"
  in
  let subset =
    let open Angstrom in
    (let* components = sep_by (string ", ") component in
     return (subset_from_components components))
    <?> "subset"
  in
  let line =
    let open Angstrom in
    (let* _ = string "Game " in
     let* id = id in
     let* _ = string ": " in
     let* subsets = sep_by (string "; ") subset in
     return { id; subsets })
    <?> "line"
  in
  Angstrom.parse_string ~consume:All line s |> Result.ok_or_failwith

let parse lines = List.map ~f:parse_line lines

let%expect_test "parse" =
  parse sample |> [%sexp_of: line list] |> print_s;
  [%expect
    {|
    (((id 1)
      (subsets
       (((red 4) (green 0) (blue 3)) ((red 1) (green 2) (blue 6))
        ((red 0) (green 2) (blue 0)))))
     ((id 2)
      (subsets
       (((red 0) (green 2) (blue 1)) ((red 1) (green 3) (blue 4))
        ((red 0) (green 1) (blue 1)))))
     ((id 3)
      (subsets
       (((red 20) (green 8) (blue 6)) ((red 4) (green 13) (blue 5))
        ((red 1) (green 5) (blue 0)))))
     ((id 4)
      (subsets
       (((red 3) (green 1) (blue 6)) ((red 6) (green 3) (blue 0))
        ((red 14) (green 3) (blue 15)))))
     ((id 5)
      (subsets (((red 6) (green 3) (blue 1)) ((red 1) (green 2) (blue 2)))))) |}]

(** XXX *)
let sum = List.fold ~f:( + ) ~init:0

let result lines =
  List.filter_map lines ~f:(fun { id; subsets } ->
      if List.for_all subsets ~f:subset_is_correct then Some id else None)
  |> sum

let%expect_test "result" = parse sample |> result |> printf "%d\n";
  [%expect {| 8 |}]
let result2 _ = 0

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_lines path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_lines path |> result2 |> printf "%d\n"
  | _ -> assert false
