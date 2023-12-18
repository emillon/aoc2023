open Base
open! Lib
open Stdio

let sample =
  [
    "2413432311323";
    "3215453535623";
    "3255245654254";
    "3446585845452";
    "4546657867536";
    "1438598798454";
    "4457876987766";
    "3637877979653";
    "4654967986887";
    "4564679986453";
    "1224686865563";
    "2546548887735";
    "4322674655533";
  ]
  |> String.concat_lines

(*

+    .xxx......
+    xx.xxx....

     34^>>>1323
     >>>35v5623

    "3432311323";
    "5453535623";


  *)

type t = int Map2d.t [@@deriving sexp]

let parse =
  let open Angstrom in
  let symbol =
    let+ c = satisfy Char.is_digit in
    Some (Char.to_int c - Char.to_int '0')
  in
  parse (Map2d.parse symbol)

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((0 0) 2) ((0 1) 3) ((0 2) 3) ((0 3) 3) ((0 4) 4) ((0 5) 1) ((0 6) 4)
     ((0 7) 3) ((0 8) 4) ((0 9) 4) ((0 10) 1) ((0 11) 2) ((0 12) 4) ((1 0) 4)
     ((1 1) 2) ((1 2) 2) ((1 3) 4) ((1 4) 5) ((1 5) 4) ((1 6) 4) ((1 7) 6)
     ((1 8) 6) ((1 9) 5) ((1 10) 2) ((1 11) 5) ((1 12) 3) ((2 0) 1) ((2 1) 1)
     ((2 2) 5) ((2 3) 4) ((2 4) 4) ((2 5) 3) ((2 6) 5) ((2 7) 3) ((2 8) 5)
     ((2 9) 6) ((2 10) 2) ((2 11) 4) ((2 12) 2) ((3 0) 3) ((3 1) 5) ((3 2) 5)
     ((3 3) 6) ((3 4) 6) ((3 5) 8) ((3 6) 7) ((3 7) 7) ((3 8) 4) ((3 9) 4)
     ((3 10) 4) ((3 11) 6) ((3 12) 2) ((4 0) 4) ((4 1) 4) ((4 2) 2) ((4 3) 5)
     ((4 4) 6) ((4 5) 5) ((4 6) 8) ((4 7) 8) ((4 8) 9) ((4 9) 6) ((4 10) 6)
     ((4 11) 5) ((4 12) 6) ((5 0) 3) ((5 1) 5) ((5 2) 4) ((5 3) 8) ((5 4) 5)
     ((5 5) 9) ((5 6) 7) ((5 7) 7) ((5 8) 6) ((5 9) 7) ((5 10) 8) ((5 11) 4)
     ((5 12) 7) ((6 0) 2) ((6 1) 3) ((6 2) 5) ((6 3) 5) ((6 4) 7) ((6 5) 8)
     ((6 6) 6) ((6 7) 7) ((6 8) 7) ((6 9) 9) ((6 10) 6) ((6 11) 8) ((6 12) 4)
     ((7 0) 3) ((7 1) 5) ((7 2) 6) ((7 3) 8) ((7 4) 8) ((7 5) 7) ((7 6) 9)
     ((7 7) 9) ((7 8) 9) ((7 9) 9) ((7 10) 8) ((7 11) 8) ((7 12) 6) ((8 0) 1)
     ((8 1) 3) ((8 2) 5) ((8 3) 4) ((8 4) 6) ((8 5) 9) ((8 6) 8) ((8 7) 7)
     ((8 8) 8) ((8 9) 8) ((8 10) 6) ((8 11) 8) ((8 12) 5) ((9 0) 1) ((9 1) 5)
     ((9 2) 4) ((9 3) 5) ((9 4) 7) ((9 5) 8) ((9 6) 7) ((9 7) 9) ((9 8) 6)
     ((9 9) 6) ((9 10) 5) ((9 11) 7) ((9 12) 5) ((10 0) 3) ((10 1) 6) ((10 2) 2)
     ((10 3) 4) ((10 4) 5) ((10 5) 4) ((10 6) 7) ((10 7) 6) ((10 8) 8) ((10 9) 4)
     ((10 10) 5) ((10 11) 7) ((10 12) 5) ((11 0) 2) ((11 1) 2) ((11 2) 5)
     ((11 3) 5) ((11 4) 3) ((11 5) 5) ((11 6) 6) ((11 7) 5) ((11 8) 8) ((11 9) 5)
     ((11 10) 6) ((11 11) 3) ((11 12) 3) ((12 0) 3) ((12 1) 3) ((12 2) 4)
     ((12 3) 2) ((12 4) 6) ((12 5) 4) ((12 6) 6) ((12 7) 3) ((12 8) 7) ((12 9) 3)
     ((12 10) 3) ((12 11) 5) ((12 12) 3)) |}]

type dir = N | S | E | W [@@deriving compare, equal, hash, sexp]

let all_dirs = [ N; S; E; W ]
let reverse_dir = function N -> S | S -> N | E -> W | W -> E

let shift (i, j) = function
  | E -> (i + 1, j)
  | W -> (i - 1, j)
  | N -> (i, j - 1)
  | S -> (i, j + 1)

let guard = function true -> Some () | false -> None

module Last_dirs : sig
  type t [@@deriving compare, equal, hash, sexp]

  val empty : t
  val ultra : t
  val next : t -> (dir * t) list
end = struct
  type t =
    | Same of { last_dir : dir; count : int; max : int; min : int }
    | Empty of { max : int; min : int }
  [@@deriving compare, equal, hash, sexp]

  let empty = Empty { min = -1; max = 3 }
  let ultra = Empty { min = 4; max = 10 }

  let add ld d =
    match ld with
    | Empty { min; max } -> Some (Same { last_dir = d; count = 1; max; min })
    | Same { last_dir; count; max; min } ->
        if equal_dir d last_dir then
          if count = max then None
          else Some (Same { last_dir; count = count + 1; max; min })
        else if count < min then None
        else Some (Same { last_dir = d; count = 1; max; min })

  let last_dir = function
    | Empty _ -> None
    | Same { last_dir; _ } -> Some last_dir

  let is_reverse t dir =
    [%equal: dir option] (last_dir t) (Some (reverse_dir dir))

  let next t =
    List.filter_map all_dirs ~f:(fun dir ->
        let open Option.Let_syntax in
        let%bind () = guard (not (is_reverse t dir)) in
        let%map t' = add t dir in
        (dir, t'))
end

module State = struct
  module T = struct
    type t = { pos : Pos.t; last_dirs : Last_dirs.t }
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

let next_states { State.pos; last_dirs } =
  Last_dirs.next last_dirs
  |> List.map ~f:(fun (dir, last_dirs) ->
         let new_pos = shift pos dir in
         { State.pos = new_pos; last_dirs })

let add_dist d n = if Int.equal d Int.max_value then d else d + n

let iter_on_heap q ~f =
  while not (Pairing_heap.is_empty q) do
    f (Pairing_heap.pop_exn q)
  done

let shortest last_dirs m =
  let bounds = Map2d.bounds m in
  let end_pos = (bounds.imax, bounds.jmax) in
  let dist = Hashtbl.create (module State) in
  let dist_f s =
    match Hashtbl.find dist s with Some d -> d | None -> Int.max_value
  in
  let compare_dist_state a b = Int.compare (dist_f a) (dist_f b) in
  let q = Pairing_heap.create ~cmp:compare_dist_state () in
  let start_pos = (0, 0) in
  let start_state = { State.pos = start_pos; last_dirs } in
  Hashtbl.set dist ~key:start_state ~data:0;
  Pairing_heap.add q start_state;
  let exception Found of State.t in
  try
    iter_on_heap q ~f:(fun u ->
        if Pos.equal u.pos end_pos then raise (Found u);
        next_states u
        |> List.iter ~f:(fun v ->
               if Map2d.in_bounds bounds v.pos then
                 let alt = add_dist (dist_f u) (Map.find_exn m v.pos) in
                 if alt < dist_f v then (
                   Hashtbl.set dist ~key:v ~data:alt;
                   Pairing_heap.add q v)));
    assert false
  with Found end_state -> Hashtbl.find_exn dist end_state

let result m = shortest Last_dirs.empty m

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {|
    102 |}]

let result2 m = shortest Last_dirs.ultra m

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 94 |}]

let run () = main All parse result result2
