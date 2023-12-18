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

type dir = N | S | E | W [@@deriving compare, equal, sexp]

let all_dirs = [ N; S; E; W ]
let reverse_dir = function N -> S | S -> N | E -> W | W -> E

let shift (i, j) = function
  | E -> (i + 1, j)
  | W -> (i - 1, j)
  | N -> (i, j - 1)
  | S -> (i, j + 1)

module Last_dirs : sig
  type t [@@deriving compare, equal, sexp]

  val empty : t
  val add : t -> dir -> t option
  val is_reverse : t -> dir -> bool
end = struct
  type t = dir option * dir option * dir option
  [@@deriving compare, equal, sexp]

  let empty = (None, None, None)

  let add ld d =
    match ld with
    | Some a, Some b, Some c
      when equal_dir a b && equal_dir a c && equal_dir a d ->
        None
    | ao, bo, _ -> Some (Some d, ao, bo)

  let is_reverse (ao, _, _) dir =
    match ao with Some a -> equal_dir a (reverse_dir dir) | None -> false
end

module State = struct
  module T = struct
    type t = { pos : Pos.t; last_dirs : Last_dirs.t }
    [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
end

let guard = function true -> Some () | false -> None

let next_states { State.pos; last_dirs } bounds =
  List.filter_map all_dirs ~f:(fun dir ->
      let open Option.Let_syntax in
      let new_pos = shift pos dir in
      let%bind () = guard (Map2d.in_bounds bounds new_pos) in
      let%bind () = guard (not (Last_dirs.is_reverse last_dirs dir)) in
      let%map last_dirs = Last_dirs.add last_dirs dir in
      { State.pos = new_pos; last_dirs })

type dist = Finite of int | Inf [@@deriving compare, equal, sexp]

let add_dist d n = match d with Inf -> Inf | Finite x -> Finite (x + n)

let%expect_test "compare_dist" =
  let test a b =
    let r = compare_dist a b in
    print_s [%message "compare_dist" (a : dist) (b : dist) (r : int)]
  in
  test Inf Inf;
  [%expect {| (compare_dist (a Inf) (b Inf) (r 0)) |}];
  test (Finite 4) (Finite 6);
  [%expect {| (compare_dist (a (Finite 4)) (b (Finite 6)) (r -1)) |}];
  test (Finite 4) Inf;
  [%expect {| (compare_dist (a (Finite 4)) (b Inf) (r -1)) |}];
  test (Finite 10) (Finite 0);
  [%expect {| (compare_dist (a (Finite 10)) (b (Finite 0)) (r 1)) |}];
  test Inf (Finite 0);
  [%expect {| (compare_dist (a Inf) (b (Finite 0)) (r 1)) |}]

let iter_on_heap q ~f =
  while not (Pairing_heap.is_empty q) do
    f (Pairing_heap.pop_exn q)
  done

let shortest m =
  let bounds = Map2d.bounds m in
  let end_pos = (bounds.imax, bounds.jmax) in
  let dist = ref (Map.empty (module State)) in
  let prev = ref (Map.empty (module State)) in
  let rec get_trace pos =
    match Map.find !prev pos with
    | None -> [ pos ]
    | Some p -> pos :: get_trace p
  in
  let dist_f s = match Map.find !dist s with Some d -> d | None -> Inf in
  let compare_dist_state a b = compare_dist (dist_f a) (dist_f b) in
  let q = Pairing_heap.create ~cmp:compare_dist_state () in
  let start_pos = (0, 0) in
  let start_state = { State.pos = start_pos; last_dirs = Last_dirs.empty } in
  dist := Map.set !dist ~key:start_state ~data:(Finite 0);
  Pairing_heap.add q start_state;
  let exception Found of State.t in
  try
    iter_on_heap q ~f:(fun u ->
        if Pos.equal u.pos end_pos then raise (Found u);
        next_states u bounds
        |> List.iter ~f:(fun v ->
               let alt = add_dist (dist_f u) (Map.find_exn m v.pos) in
               if compare_dist alt (dist_f v) < 0 then (
                 dist := Map.set !dist ~key:v ~data:alt;
                 prev := Map.set !prev ~key:v ~data:u;
                 Pairing_heap.add q v)));
    assert false
  with Found end_state -> (Map.find_exn !dist end_state, get_trace end_state)

let result m =
  let d, _t = shortest m in
  match d with Finite n -> n | Inf -> assert false

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect
    {|
    102 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
