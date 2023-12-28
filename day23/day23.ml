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

module E = struct
  type t = int [@@deriving compare]

  let default = 1
end

module G = Graph.Persistent.Graph.ConcreteLabeled (Pos) (E)

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
      next_states t u
      |> List.iter ~f:(fun v ->
             let alt = add_dist (dist_f u) (-1) in
             if alt < dist_f v then (
               if false then
                 print_s [%message "better score" (v : State.t) (alt : int)];
               Hashtbl.set dist ~key:v.pos ~data:alt;
               Pairing_heap.add q v)));
  -Hashtbl.find_exn dist end_pos

let contract_edges g u e1 e2 =
  let _, w1, dst1 = e1 in
  let _, w2, dst2 = e2 in
  let acc = g in
  let acc = G.add_edge_e acc (dst1, w1 + w2, dst2) in
  let acc = G.remove_edge_e acc e1 in
  let acc = G.remove_edge_e acc e2 in
  let acc = G.remove_vertex acc u in
  acc

let contract_one g =
  let exception Go of G.t in
  match
    G.iter_vertex
      (fun u ->
        match G.succ_e g u with
        | [ e1; e2 ] -> raise (Go (contract_edges g u e1 e2))
        | _ -> ())
      g
  with
  | () -> None
  | exception Go g -> Some g

let rec contract g =
  match contract_one g with None -> g | Some g' -> contract g'

let build_graph t =
  let g =
    Map2d.Dense.fold_option t ~init:G.empty ~f:(fun ~key:u ~data:vo acc ->
        match vo with
        | Some Wall -> acc
        | Some (Slope _) -> assert false
        | None ->
            let next = next_pos t u in
            List.fold next ~init:acc ~f:(fun acc v -> G.add_edge acc u v))
  in
  contract g

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {|
    94 |}]

let rec paths g start_pos end_pos visited =
  let next =
    G.succ_e g start_pos
    |> List.filter_map ~f:(fun (_, w, p) ->
           if Set.mem visited p then None else Some (p, w))
  in
  if List.is_empty next && Pos.equal start_pos end_pos then [ 0 ]
  else
    List.concat_map next ~f:(fun (n, w) ->
        let ts = paths g n end_pos (Set.add visited n) in
        List.map ts ~f:(fun n -> n + w))

let remove_slopes =
  Map2d.Dense.mapi_option ~f:(fun _ vo ->
      match vo with
      | None -> None
      | Some Wall -> Some Wall
      | Some (Slope _) -> None)

let result2 t =
  let t = remove_slopes t in
  let g = build_graph t in
  let start_pos = (1, 0) in
  let { Map2d.imax; jmax; _ } = Map2d.Dense.bounds t in
  let end_pos = (imax - 1, jmax) in
  paths g start_pos end_pos (Set.singleton (module Pos) start_pos)
  |> List.max_elt ~compare:[%compare: int]
  |> Option.value_exn

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {|
    154 |}]

let run () = main All parse result result2
