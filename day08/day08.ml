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

type t = { moves : move list; move_len : int; nodes : node Map.M(String).t }
[@@deriving sexp]

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
    { moves; nodes; move_len = List.length moves }
  in
  Angstrom.parse_string ~consume:All t s |> Result.ok_or_failwith

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    ((moves (R L)) (move_len 2)
     (nodes
      ((AAA ((left BBB) (right CCC))) (BBB ((left DDD) (right EEE)))
       (CCC ((left ZZZ) (right GGG))) (DDD ((left DDD) (right DDD)))
       (EEE ((left EEE) (right EEE))) (GGG ((left GGG) (right GGG)))
       (ZZZ ((left ZZZ) (right ZZZ)))))) |}]

module State = struct
  module T = struct
    type t = { pos : string; index : int } [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let next t move pos =
  let { left; right } = Map.find_exn t.nodes pos in
  match move with L -> left | R -> right

let next_state t move { State.pos; index } =
  let next_pos = next t move pos in
  let next_index = (index + 1) % t.move_len in
  { State.pos = next_pos; index = next_index }

let result_gen t ~start_nodes ~end_nodes =
  let is_end_node node = List.mem end_nodes node ~equal:String.equal in
  let rec go n cur = function
    | [] -> go n cur t.moves
    | move :: moves ->
        if List.for_all cur ~f:is_end_node then n
        else
          let next = List.map cur ~f:(next t move) in
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

(** Return g, s, t and gcd(a,b) such that as+bt = g = gcd(a,b) *)
let rec egcd a b =
  if b = 0 then (a, 1, 0)
  else
    let d, s, t = egcd b (a % b) in
    (d, t, s - (a / b * t))

let%expect_test "egcd" =
  let test a b = egcd a b |> [%sexp_of: int * int * int] |> print_s in
  test 5 3;
  [%expect {| (1 -1 2) |}]

let find_cycle_info t start end_nodes =
  let rec go cur visited i moves =
    match moves with
    | [] -> go cur visited i t.moves
    | move :: moves -> (
        let next = next_state t move cur in
        match Map.find visited next with
        | Some n ->
            let off = n in
            let length = i - n in
            let end_offs =
              Map.to_alist visited
              |> List.filter_map ~f:(fun ({ pos; _ }, n) ->
                     if List.mem end_nodes pos ~equal:String.equal then Some n
                     else None)
            in
            let value = i in
            let modulus = i - off in
            if false then
              print_s
                [%message
                  "find_cycle_info"
                    (value : int)
                    (modulus : int)
                    (off : int)
                    (length : int)
                    (end_offs : int list)];
            (Z.of_int value, Z.of_int modulus)
        | None -> go next (Map.add_exn visited ~key:next ~data:i) (i + 1) moves)
  in
  let start_state = { State.pos = start; index = 0 } in
  go start_state (Map.singleton (module State) start_state 0) 0 []

let sexp_of_z z = [%sexp_of: string] (Z.to_string z)

let result2 t =
  let all_nodes = Map.keys t.nodes in
  let all_that_ends_with suffix =
    List.filter_map all_nodes ~f:(fun pos ->
        if String.is_suffix pos ~suffix then Some pos else None)
  in
  let start_nodes = all_that_ends_with "A" in
  let end_nodes = all_that_ends_with "Z" in
  print_s
    [%message "nodes" (start_nodes : string list) (end_nodes : string list)];
  let data =
    List.map start_nodes ~f:(fun start -> find_cycle_info t start end_nodes)
  in
  let prod = List.fold ~init:Z.one ~f:(fun acc (_, x) -> Z.mul acc x) data in
  List.fold data ~init:Z.zero ~f:(fun acc (ai, ni) ->
      let xNi = Z.div prod ni in
      let gcd, xMi, _ = Z.gcdext xNi ni in
      if not (Z.equal gcd Z.one) then
        raise_s [%message "not coprime" (xNi : z) (ni : z)];
      Z.(acc + (ai * xMi * xNi)))
  |> Z.to_int

(*let%expect_test "result2" =*)
(*parse sample_p2 |> result2 |> printf "%d\n";*)
(*[%expect*)
(*{|*)
  (*(nodes (start_nodes (11A 22A)) (end_nodes (11Z 22Z)))*)
  (*(find_cycle_info (off 0) (length 2) (end_offs (1)))*)
  (*(find_cycle_info (off 0) (length 6) (end_offs (5 2)))*)
  (*0 |}]*)

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_all path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_all path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
