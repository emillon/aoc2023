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

let conditions_to_string l =
  List.map l ~f:condition_to_char |> String.of_char_list

type conditions = condition list [@@deriving of_sexp]

let sexp_of_conditions l = conditions_to_string l |> [%sexp_of: string]

type line = { conditions : conditions; groups : int list } [@@deriving sexp]

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

let groups l =
  let state, r =
    List.fold l ~init:(None, []) ~f:(fun (state, r) c ->
        match (state, c) with
        | _, Unknown -> assert false
        | None, Operational -> (None, r)
        | None, Damaged -> (Some 1, r)
        | Some n, Damaged -> (Some (n + 1), r)
        | Some n, Operational -> (None, n :: r))
  in
  match state with None -> List.rev r | Some n -> List.rev (n :: r)

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
  let r = ref 0 in
  let wrap x l = (x :: l) @ [ x ] in
  let re1 =
    List.map expected_groups ~f:(fun n -> Re.repn (Re.char '#') n (Some n))
    |> List.intersperse ~sep:(Re.rep1 (Re.char '.'))
    |> wrap (Re.rep (Re.char '.'))
    |> Re.seq |> Re.whole_string
  in
  let re = Re.compile re1 in
  iter_values conditions ~f:(fun l ->
      let matches_groups l expected_groups =
        if false then [%equal: int list] (groups l) expected_groups
        else
          let s = conditions_to_string l in
          Re.execp re s
      in
      if matches_groups l expected_groups then Int.incr r);
  !r

let result l = List.map l ~f:result_line |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 21 |}]

let expand_conditions l =
  List.concat
    [ l; [ Unknown ]; l; [ Unknown ]; l; [ Unknown ]; l; [ Unknown ]; l ]

let expand_groups l = List.concat [ l; l; l; l; l ]

let expand_line { conditions; groups } =
  { conditions = expand_conditions conditions; groups = expand_groups groups }

let expand = List.map ~f:expand_line

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

(** XXX *)
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
