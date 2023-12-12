open Base
open! Lib
open Stdio

let sample =
  [
    "???.### 1,1,3";
    ".??..??...?##. 1,1,3";
    "?#?#?#?#?#?#?#? 1,3,1,6";
    "????.#...#... 4,1,1";
    "????.######..#####. 1,6,5";
    "?###???????? 3,2,1";
  ]

type condition = Operational | Damaged | Unknown [@@deriving equal, sexp]

let condition_of_char = function
  | '?' -> Unknown
  | '.' -> Operational
  | '#' -> Damaged
  | c -> raise_s [%message "condition_of_char" (c : char)]

let condition_to_char = function
  | Unknown -> '?'
  | Operational -> '.'
  | Damaged -> '#'

type line = { conditions : condition list; groups : int list } [@@deriving sexp]

let conditions line = line.conditions

type t = line list [@@deriving sexp]

let parse_line s =
  let open Angstrom_helpers in
  let open Angstrom in
  let conditions =
    let+ s = take_till Char.is_whitespace <* char ' ' in
    String.to_list s |> List.map ~f:condition_of_char
  in
  let groups = sep_by1 (char ',') number in
  let line =
    let+ conditions and+ groups in
    { conditions; groups }
  in
  parse_string ~consume:All line s |> Result.ok_or_failwith

let parse = List.map ~f:parse_line

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((conditions (Unknown Unknown Unknown Operational Damaged Damaged Damaged))
      (groups (1 1 3)))
     ((conditions
       (Operational Unknown Unknown Operational Operational Unknown Unknown
        Operational Operational Operational Unknown Damaged Damaged Operational))
      (groups (1 1 3)))
     ((conditions
       (Unknown Damaged Unknown Damaged Unknown Damaged Unknown Damaged Unknown
        Damaged Unknown Damaged Unknown Damaged Unknown))
      (groups (1 3 1 6)))
     ((conditions
       (Unknown Unknown Unknown Unknown Operational Damaged Operational
        Operational Operational Damaged Operational Operational Operational))
      (groups (4 1 1)))
     ((conditions
       (Unknown Unknown Unknown Unknown Operational Damaged Damaged Damaged
        Damaged Damaged Damaged Operational Operational Damaged Damaged Damaged
        Damaged Damaged Operational))
      (groups (1 6 5)))
     ((conditions
       (Unknown Damaged Damaged Damaged Unknown Unknown Unknown Unknown Unknown
        Unknown Unknown Unknown))
      (groups (3 2 1)))) |}]

let values_for = function
  | (Operational | Damaged) as c -> [ c ]
  | Unknown -> [ Operational; Damaged ]

let rec values = function
  | [] -> [ [] ]
  | c :: cs ->
      let open List.Let_syntax in
      let hs = values_for c in
      let%bind t = values cs in
      let%map h = hs in
      h :: t

let to_string l = List.map l ~f:condition_to_char |> String.of_char_list

let%expect_test "values" =
  parse sample |> List.hd_exn |> conditions |> values |> List.map ~f:to_string
  |> List.iter ~f:(printf "%s\n");
  [%expect
    {|
    ....###
    #...###
    .#..###
    ##..###
    ..#.###
    #.#.###
    .##.###
    ###.### |}]

let groups l =
  List.group l ~break:(fun a b -> not (equal_condition a b))
  |> List.filter ~f:(fun group -> equal_condition (List.hd_exn group) Damaged)
  |> List.map ~f:List.length

let%expect_test "groups" =
  let test s =
    parse_line (s ^ " 1")
    |> conditions |> groups |> [%sexp_of: int list] |> print_s
  in
  test "#...###";
  [%expect "(1 3)"];
  test "#.#.###";
  [%expect "(1 1 3)"];
  test ".##.###";
  [%expect "(2 3)"]

let result_line { conditions; groups = expected_groups } =
  values conditions
  |> List.count ~f:(fun l -> [%equal: int list] (groups l) expected_groups)

let result l = List.map l ~f:result_line |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 21 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_lines path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_lines path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
