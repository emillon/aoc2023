open Base
open! Lib
open Stdio

let sample =
  [
    "RL";
    "";
    "AAA = (BBB, CCC)";
    "BBB = (DDD, EEE)";
    "CCC = (ZZZ, GGG)";
    "DDD = (DDD, DDD)";
    "EEE = (EEE, EEE)";
    "GGG = (GGG, GGG)";
    "ZZZ = (ZZZ, ZZZ)";
  ]
  |> String.concat_lines

type move = L | R [@@deriving sexp]
type node = { left : string; right : string } [@@deriving sexp]
type t = { moves : move list; nodes : node Map.M(String).t } [@@deriving sexp]

let parse =
  let open Angstrom in
  let move = choice [ char 'L' *> return L; char 'R' *> return R ] in
  let moves = many1 move <* end_of_line <* end_of_line in
  let name = take_while Char.is_alphanum in
  let node =
    let+ pos = name <* string " = ("
    and+ left = name <* string ", "
    and+ right = name <* string ")" <* end_of_line in
    (pos, { left; right })
  in
  let t =
    let+ moves and+ node_list = many node in
    let nodes = Map.of_alist_exn (module String) node_list in
    { moves; nodes }
  in
  parse t

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    ((moves (R L))
     (nodes
      ((AAA ((left BBB) (right CCC))) (BBB ((left DDD) (right EEE)))
       (CCC ((left ZZZ) (right GGG))) (DDD ((left DDD) (right DDD)))
       (EEE ((left EEE) (right EEE))) (GGG ((left GGG) (right GGG)))
       (ZZZ ((left ZZZ) (right ZZZ)))))) |}]

let next t move pos =
  let { left; right } = Map.find_exn t.nodes pos in
  match move with L -> left | R -> right

let result t =
  let is_end_node node = String.equal "ZZZ" node in
  let rec go n cur = function
    | [] -> go n cur t.moves
    | move :: moves ->
        if is_end_node cur then n
        else
          let next = next t move cur in
          go (n + 1) next moves
  in
  go 0 "AAA" []

let sample2 =
  [ "LLR"; ""; "AAA = (BBB, BBB)"; "BBB = (AAA, ZZZ)"; "ZZZ = (ZZZ, ZZZ)" ]
  |> String.concat_lines

let%expect_test "result" =
  let test x = parse x |> result |> printf "%d\n" in
  test sample;
  [%expect {| 2 |}];
  test sample2;
  [%expect {| 6 |}]

let sample_p2 =
  [
    "LR";
    "";
    "11A = (11B, XXX)";
    "11B = (XXX, 11Z)";
    "11Z = (11B, XXX)";
    "22A = (22B, XXX)";
    "22B = (22C, 22C)";
    "22C = (22Z, 22Z)";
    "22Z = (22B, 22B)";
    "XXX = (XXX, XXX)";
  ]
  |> String.concat_lines

let find_cycle_info t start end_nodes =
  let prev_end = ref None in
  let rec go cur i moves =
    match moves with
    | [] -> go cur i t.moves
    | move :: moves ->
        let continue () =
          let next = next t move cur in
          go next (i + 1) moves
        in
        if List.mem end_nodes cur ~equal:String.equal then (
          match !prev_end with
          | Some n -> i - n
          | None ->
              prev_end := Some i;
              continue ())
        else continue ()
  in
  go start 0 []

let result2 t =
  let all_nodes = Map.keys t.nodes in
  let all_that_ends_with suffix =
    List.filter_map all_nodes ~f:(fun pos ->
        if String.is_suffix pos ~suffix then Some pos else None)
  in
  let start_nodes = all_that_ends_with "A" in
  let end_nodes = all_that_ends_with "Z" in
  let lengths =
    List.map start_nodes ~f:(fun start -> find_cycle_info t start end_nodes)
  in
  lcm lengths

let%expect_test "result2" =
  parse sample_p2 |> result2 |> printf "%d\n";
  [%expect {| 6 |}]

let run () = main All parse result result2
