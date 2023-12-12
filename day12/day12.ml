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

type condition = Operational | Damaged | Unknown
[@@deriving compare, equal, sexp]

let condition_of_char = function
  | '?' -> Unknown
  | '.' -> Operational
  | '#' -> Damaged
  | c -> raise_s [%message "condition_of_char" (c : char)]

let condition_to_char = function
  | Unknown -> '?'
  | Operational -> '.'
  | Damaged -> '#'

let conditions_to_string l =
  List.map l ~f:condition_to_char |> String.of_char_list

type conditions = condition list [@@deriving compare, of_sexp]

let sexp_of_conditions l = conditions_to_string l |> [%sexp_of: string]

type line = { conditions : conditions; groups : int list }
[@@deriving compare, sexp]

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
    (((conditions ???.###) (groups (1 1 3)))
     ((conditions .??..??...?##.) (groups (1 1 3)))
     ((conditions ?#?#?#?#?#?#?#?) (groups (1 3 1 6)))
     ((conditions ????.#...#...) (groups (4 1 1)))
     ((conditions ????.######..#####.) (groups (1 6 5)))
     ((conditions ?###????????) (groups (3 2 1)))) |}]

let values_for = function
  | (Operational | Damaged) as c -> [ c ]
  | Unknown -> [ Operational; Damaged ]

let rec iter_values ~f = function
  | [] -> f []
  | c :: cs ->
      let hs = values_for c in
      iter_values cs ~f:(fun t -> List.iter hs ~f:(fun h -> f (h :: t)))

let values_for_test l =
  let r = ref [] in
  iter_values l ~f:(fun x -> r := x :: !r);
  List.rev !r

let%expect_test "values" =
  parse sample |> List.hd_exn |> conditions |> values_for_test
  |> [%sexp_of: conditions list] |> print_s;
  [%expect
    {|
    (....### #...### .#..### ##..### ..#.### #.#.### .##.### ###.###) |}]

let group_match l expected =
  let rec go state expected cs =
    match (state, expected, cs) with
    | _, _, Unknown :: _ -> assert false
    | None, expected, Damaged :: cs -> go (Some 1) expected cs
    | None, expected, Operational :: cs -> go None expected cs
    | None, [], [] -> true
    | None, _ :: _, [] -> false
    | Some n, expected, Damaged :: cs -> go (Some (n + 1)) expected cs
    | Some n, expected_n :: expected, Operational :: cs ->
        expected_n = n && go None expected cs
    | Some n, [ expected_n ], [] -> expected_n = n
    | Some _, _ :: _ :: _, [] -> false
    | Some _, [], Operational :: _ -> false
    | Some _, [], [] -> false
  in
  go None expected l

let map_head ~f = function [] -> [] | x :: xs -> f x :: xs

let pairings t =
  let rec go condition_groups groups =
    match (condition_groups, groups) with
    | [ conditions ], groups -> [ [ { conditions; groups } ] ]
    | condition_groups, [] ->
        [
          List.map condition_groups ~f:(fun conditions ->
              { conditions; groups = [] });
        ]
    | [], _ -> assert false
    | condition_group :: condition_groups, group :: groups ->
        (* does first group match first condition group? *)
        (* a:
           yes.
           pair all condition groups with other groups and
           add group to all beginnings.
        *)
        let a =
          let r = go (condition_group :: condition_groups) groups in
          List.map r ~f:(fun t ->
              map_head t ~f:(fun line ->
                  { line with groups = group :: line.groups }))
        in
        (* b: no.
            pair other condition groups with all groups and
            and add an empty match
        *)
        let b =
          let r = go condition_groups (group :: groups) in
          let first_group = { groups = []; conditions = condition_group } in
          List.map r ~f:(fun l -> first_group :: l)
        in
        a @ b
  in
  let condition_groups =
    let break a b =
      match (a, b) with
      | Operational, _ -> true
      | _, Operational -> true
      | _ -> false
    in
    List.group t.conditions ~break
    |> List.filter ~f:(function [ Operational ] -> false | _ -> true)
  in
  if false then
    print_s [%message "pairings" (condition_groups : conditions list)];
  go condition_groups t.groups

let%expect_test "pairings" =
  let test s =
    parse_line s |> pairings |> [%sexp_of: line list list] |> print_s
  in
  test "?.# 1,3";
  [%expect
    {|
    ((((conditions ?) (groups (1 3))) ((conditions #) (groups ())))
     (((conditions ?) (groups (1))) ((conditions #) (groups (3))))
     (((conditions ?) (groups ())) ((conditions #) (groups (1 3))))) |}];
  test "???????..??? 2,1,1";
  [%expect
    {|
    ((((conditions ???????) (groups (2 1 1))) ((conditions ???) (groups ())))
     (((conditions ???????) (groups (2 1))) ((conditions ???) (groups (1))))
     (((conditions ???????) (groups (2))) ((conditions ???) (groups (1 1))))
     (((conditions ???????) (groups ())) ((conditions ???) (groups (2 1 1))))) |}];
  test "?.?.??.??? 1,3";
  [%expect
    {|
    ((((conditions ?) (groups (1 3))) ((conditions ?) (groups ()))
      ((conditions ??) (groups ())) ((conditions ???) (groups ())))
     (((conditions ?) (groups (1))) ((conditions ?) (groups (3)))
      ((conditions ??) (groups ())) ((conditions ???) (groups ())))
     (((conditions ?) (groups (1))) ((conditions ?) (groups ()))
      ((conditions ??) (groups (3))) ((conditions ???) (groups ())))
     (((conditions ?) (groups (1))) ((conditions ?) (groups ()))
      ((conditions ??) (groups ())) ((conditions ???) (groups (3))))
     (((conditions ?) (groups ())) ((conditions ?) (groups (1 3)))
      ((conditions ??) (groups ())) ((conditions ???) (groups ())))
     (((conditions ?) (groups ())) ((conditions ?) (groups (1)))
      ((conditions ??) (groups (3))) ((conditions ???) (groups ())))
     (((conditions ?) (groups ())) ((conditions ?) (groups (1)))
      ((conditions ??) (groups ())) ((conditions ???) (groups (3))))
     (((conditions ?) (groups ())) ((conditions ?) (groups ()))
      ((conditions ??) (groups (1 3))) ((conditions ???) (groups ())))
     (((conditions ?) (groups ())) ((conditions ?) (groups ()))
      ((conditions ??) (groups (1))) ((conditions ???) (groups (3))))
     (((conditions ?) (groups ())) ((conditions ?) (groups ()))
      ((conditions ??) (groups ())) ((conditions ???) (groups (1 3))))) |}]

let expand_conditions l =
  List.concat
    [ l; [ Unknown ]; l; [ Unknown ]; l; [ Unknown ]; l; [ Unknown ]; l ]

let expand_groups l = List.concat [ l; l; l; l; l ]

let expand_line { conditions; groups } =
  { conditions = expand_conditions conditions; groups = expand_groups groups }

let expand = List.map ~f:expand_line

let result_line { conditions; groups } =
  let go () =
    let r = ref 0 in
    iter_values conditions ~f:(fun l -> if group_match l groups then Int.incr r);
    !r
  in
  let group_total = sum groups in
  let maximum = List.length conditions in
  if group_total > maximum then 0
  else
    let minimum =
      List.count conditions ~f:(function Damaged -> true | _ -> false)
    in
    if group_total < minimum then 0 else go ()

let result_line_opt l =
  pairings l
  |> List.map ~f:(fun l -> List.map l ~f:result_line |> product)
  |> sum

let result l = List.map l ~f:result_line_opt |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 21 |}]

let%expect_test "expand" =
  parse sample |> expand |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((conditions ???.###????.###????.###????.###????.###)
      (groups (1 1 3 1 1 3 1 1 3 1 1 3 1 1 3)))
     ((conditions
       .??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##.)
      (groups (1 1 3 1 1 3 1 1 3 1 1 3 1 1 3)))
     ((conditions
       ?#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#?)
      (groups (1 3 1 6 1 3 1 6 1 3 1 6 1 3 1 6 1 3 1 6)))
     ((conditions
       ????.#...#...?????.#...#...?????.#...#...?????.#...#...?????.#...#...)
      (groups (4 1 1 4 1 1 4 1 1 4 1 1 4 1 1)))
     ((conditions
       ????.######..#####.?????.######..#####.?????.######..#####.?????.######..#####.?????.######..#####.)
      (groups (1 6 5 1 6 5 1 6 5 1 6 5 1 6 5)))
     ((conditions
       ?###??????????###??????????###??????????###??????????###????????)
      (groups (3 2 1 3 2 1 3 2 1 3 2 1 3 2 1)))) |}]

(*let result2 t = expand t |> result*)

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
