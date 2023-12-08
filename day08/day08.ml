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

let parse s =
  let move =
    let open Angstrom in
    choice [ char 'L' *> return L; char 'R' *> return R ]
  in
  let moves =
    let open Angstrom in
    many1 move <* end_of_line <* end_of_line
  in
  let name =
    let open Angstrom in
    take_while Char.is_alphanum
  in
  let node =
    let open Angstrom in
    let+ pos = name <* string " = ("
    and+ left = name <* string ", "
    and+ right = name <* string ")" <* end_of_line in
    (pos, { left; right })
  in
  let t =
    let open Angstrom in
    let+ moves and+ node_list = many node in
    let nodes = Map.of_alist_exn (module String) node_list in
    { moves; nodes }
  in
  Angstrom.parse_string ~consume:All t s |> Result.ok_or_failwith

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

let result_gen t ~start_nodes ~end_nodes =
  let is_end_node node = List.mem end_nodes node ~equal:String.equal in
  let rec go n cur = function
    | [] -> go n cur t.moves
    | move :: moves ->
        if List.for_all cur ~f:is_end_node then n
        else
          let next =
            List.map cur ~f:(fun c ->
                let { left; right } = Map.find_exn t.nodes c in
                match move with L -> left | R -> right)
          in
          go (n + 1) next moves
  in
  go 0 start_nodes t.moves

let result t = result_gen t ~start_nodes:[ "AAA" ] ~end_nodes:[ "ZZZ" ]

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

let result2 t =
  let all_nodes = Map.keys t.nodes in
  let all_that_ends_with suffix =
    List.filter_map all_nodes ~f:(fun pos ->
        if String.is_suffix pos ~suffix then Some pos else None)
  in
  let start_nodes = all_that_ends_with "A" in
  let end_nodes = all_that_ends_with "Z" in
  result_gen t ~start_nodes ~end_nodes

let%expect_test "result2" =
  parse sample_p2 |> result2 |> printf "%d\n";
  [%expect {| 6 |}]

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_all path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_all path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
