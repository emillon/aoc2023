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
type node = { pos : string; left : string; right : string } [@@deriving sexp]
type t = { moves : move list; nodes : node list } [@@deriving sexp]

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
    take_while Char.is_alpha
  in
  let node =
    let open Angstrom in
    let+ pos = name <* string " = ("
    and+ left = name <* string ", "
    and+ right = name <* string ")" <* end_of_line in
    { pos; left; right }
  in
  let t =
    let open Angstrom in
    let+ moves and+ nodes = many node in
    { moves; nodes }
  in
  Angstrom.parse_string ~consume:All t s |> Result.ok_or_failwith

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    ((moves (R L))
     (nodes
      (((pos AAA) (left BBB) (right CCC)) ((pos BBB) (left DDD) (right EEE))
       ((pos CCC) (left ZZZ) (right GGG)) ((pos DDD) (left DDD) (right DDD))
       ((pos EEE) (left EEE) (right EEE)) ((pos GGG) (left GGG) (right GGG))
       ((pos ZZZ) (left ZZZ) (right ZZZ))))) |}]

let result t =
  let rec go n cur = function
    | [] -> go n cur t.moves
    | move :: moves ->
        if String.equal cur "ZZZ" then n
        else
          let node =
            List.find_exn t.nodes ~f:(fun { pos; _ } -> String.equal pos cur)
          in
          let next = match move with L -> node.left | R -> node.right in
          go (n + 1) next moves
  in
  go 0 "AAA" t.moves

let sample2 =
  [ "LLR"; ""; "AAA = (BBB, BBB)"; "BBB = (AAA, ZZZ)"; "ZZZ = (ZZZ, ZZZ)" ]
  |> String.concat_lines

let%expect_test "result" =
  let test x = parse x |> result |> printf "%d\n" in
  test sample;
  [%expect {| 2 |}];
  test sample2;
  [%expect {| 6 |}]

let result2 _ = 0

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_all path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_all path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
