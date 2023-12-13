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

type t = line list [@@deriving sexp]

let parse_line s =
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

let expand_conditions l =
  List.concat
    [ l; [ Unknown ]; l; [ Unknown ]; l; [ Unknown ]; l; [ Unknown ]; l ]

let expand_groups l = List.concat [ l; l; l; l; l ]

let expand_line { conditions; groups } =
  { conditions = expand_conditions conditions; groups = expand_groups groups }

let expand = List.map ~f:expand_line

let result_line_new ~f { conditions; groups } =
  match groups with
  | [] -> if List.mem conditions Damaged ~equal:equal_condition then 0 else 1
  | current :: groups ->
      List.range 0
        (List.length conditions - sum groups - List.length groups - current)
        ~stop:`inclusive
      |> List.take_while ~f:(fun i ->
             not
               (List.existsi conditions ~f:(fun j c ->
                    match c with Damaged -> j < i | _ -> false)))
      |> List.filter_map ~f:(fun i ->
             let next = i + current in
             if
               next <= List.length conditions
               && (not
                     (List.existsi conditions ~f:(fun j c ->
                          match c with
                          | Operational -> i <= j && j < next
                          | _ -> false)))
               && not
                    ([%equal: condition option] (List.nth conditions next)
                       (Some Damaged))
             then
               Some (f { conditions = List.drop conditions (next + 1); groups })
             else None)
      |> sum

module Line = struct
  module T = struct
    type t = line [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let rec result_line_new_cached =
  let cache = ref (Map.empty (module Line)) in
  fun line ->
    match Map.find !cache line with
    | Some r -> r
    | None ->
        let data = result_line_new ~f:result_line_new_cached line in
        cache := Map.add_exn !cache ~key:line ~data;
        data

let result l = List.map l ~f:result_line_new_cached |> sum

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

let result2 t = expand t |> result

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 525152 |}]

let run () = main Lines parse result result2
