open Base
open! Lib
open Stdio

let sample =
  [
    "#.#####################";
    "#.......#########...###";
    "#######.#########.#.###";
    "###.....#.>.>.###.#.###";
    "###v#####.#v#.###.#.###";
    "###.>...#.#.#.....#...#";
    "###v###.#.#.#########.#";
    "###...#.#.#.......#...#";
    "#####.#.#.#######.#.###";
    "#.....#.#.#.......#...#";
    "#.#####.#.#.#########v#";
    "#.#...#...#...###...>.#";
    "#.#.#v#######v###.###v#";
    "#...#.>.#...>.>.#.###.#";
    "#####v#.#.###v#.#.###.#";
    "#.....#...#...#.#.#...#";
    "#.#########.###.#.#.###";
    "#...###...#...#...#.###";
    "###.###.#.###v#####v###";
    "#...#...#.#.>.>.#.>.###";
    "#.###.###.#.###.#.#v###";
    "#.....###...###...#...#";
    "#####################.#";
  ]
  |> String.concat_lines

type symbol = Wall | Slope of Dir.t [@@deriving sexp]
type t = symbol Map2d.Dense.t [@@deriving sexp]

let parse =
  let open Angstrom in
  let symbol =
    choice
      [
        char '.' *> return None;
        char '#' *> return (Some Wall);
        char '>' *> return (Some (Slope E));
        char '<' *> return (Some (Slope W));
        char '^' *> return (Some (Slope N));
        char 'v' *> return (Some (Slope S));
      ]
  in
  parse (Map2d.Dense.parse symbol)

let view ?sets t =
  Map2d.Dense.view ?sets t (function
    | Wall -> "▓"
    | Slope E -> "→"
    | Slope W -> "←"
    | Slope N -> "↑"
    | Slope S -> "↓")

let%expect_test "parse" =
  parse sample |> view;
  [%expect
    {|
    ▓.▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
    ▓.......▓▓▓▓▓▓▓▓▓...▓▓▓
    ▓▓▓▓▓▓▓.▓▓▓▓▓▓▓▓▓.▓.▓▓▓
    ▓▓▓.....▓.→.→.▓▓▓.▓.▓▓▓
    ▓▓▓↓▓▓▓▓▓.▓↓▓.▓▓▓.▓.▓▓▓
    ▓▓▓.→...▓.▓.▓.....▓...▓
    ▓▓▓↓▓▓▓.▓.▓.▓▓▓▓▓▓▓▓▓.▓
    ▓▓▓...▓.▓.▓.......▓...▓
    ▓▓▓▓▓.▓.▓.▓▓▓▓▓▓▓.▓.▓▓▓
    ▓.....▓.▓.▓.......▓...▓
    ▓.▓▓▓▓▓.▓.▓.▓▓▓▓▓▓▓▓▓↓▓
    ▓.▓...▓...▓...▓▓▓...→.▓
    ▓.▓.▓↓▓▓▓▓▓▓▓↓▓▓▓.▓▓▓↓▓
    ▓...▓.→.▓...→.→.▓.▓▓▓.▓
    ▓▓▓▓▓↓▓.▓.▓▓▓↓▓.▓.▓▓▓.▓
    ▓.....▓...▓...▓.▓.▓...▓
    ▓.▓▓▓▓▓▓▓▓▓.▓▓▓.▓.▓.▓▓▓
    ▓...▓▓▓...▓...▓...▓.▓▓▓
    ▓▓▓.▓▓▓.▓.▓▓▓↓▓▓▓▓▓↓▓▓▓
    ▓...▓...▓.▓.→.→.▓.→.▓▓▓
    ▓.▓▓▓.▓▓▓.▓.▓▓▓.▓.▓↓▓▓▓
    ▓.....▓▓▓...▓▓▓...▓...▓
    ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓.▓ |}]

(* XXX *)
let iter_on_heap q ~f =
  while not (Pairing_heap.is_empty q) do
    f (Pairing_heap.pop_exn q)
  done

let next_pos t u =
  match Map2d.Dense.get t u with
  | Some (Slope d) -> [ Dir.shift u d ]
  | Some Wall -> assert false
  | None ->
      List.filter_map Dir.all ~f:(fun d ->
          let v = Dir.shift u d in
          if Map2d.in_bounds (Map2d.Dense.bounds t) v then
            match Map2d.Dense.get t v with
            | None -> Some v
            | Some (Slope _) -> Some v
            | Some Wall -> None
          else None)

module State = struct
  type t = { pos : Pos.t; visited : Set.M(Pos).t }
  [@@deriving compare, hash, sexp]
end

let next_states t u =
  next_pos t u.State.pos
  |> List.filter_map ~f:(fun pos ->
         if Set.mem u.visited pos then None
         else Some { State.pos; visited = Set.add u.visited pos })

let add_dist d n = if Int.equal d Int.max_value then d else d + n

let result t =
  let dist = Hashtbl.create (module Pos) in
  let dist_f s =
    match Hashtbl.find dist s.State.pos with
    | Some d -> d
    | None -> Int.max_value
  in
  let compare_dist a b = Int.compare (dist_f a) (dist_f b) in
  let q = Pairing_heap.create ~cmp:compare_dist () in
  let { Map2d.imax; jmax; _ } = Map2d.Dense.bounds t in
  let start_pos = (1, 0) in
  let start_state =
    { State.pos = start_pos; visited = Set.singleton (module Pos) start_pos }
  in
  let end_pos = (imax - 1, jmax) in
  Hashtbl.set dist ~key:start_pos ~data:0;
  Pairing_heap.add q start_state;
  iter_on_heap q ~f:(fun u ->
      (*if Pos.equal u.pos end_pos then raise (Found u);*)
      let next = next_states t u in
      (*print_s [%message (u.pos : Pos.t) (next : Pos.t list)];*)
      (*assert (not (List.is_empty next));*)
      List.iter next ~f:(fun v ->
          let alt = add_dist (dist_f u) (-1) in
          if alt < dist_f v then (
            if false then
              print_s [%message "better score" (v : State.t) (alt : int)];
            Hashtbl.set dist ~key:v.pos ~data:alt;
            Pairing_heap.add q v)));
  -Hashtbl.find_exn dist end_pos

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {|
    94 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
