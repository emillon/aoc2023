open Base
open! Lib
open Stdio

let sample =
  [
    {|.|...\....|};
    {||.-.\.....|};
    {|.....|-...|};
    {|........|.|};
    {|..........|};
    {|.........\|};
    {|..../.\\..|};
    {|.-.-/..|..|};
    {|.|....-|.\|};
    {|..//.|....|};
  ]
  |> String.concat_lines

type mirror = NW | NE [@@deriving sexp]
type splitter = NS | EW [@@deriving sexp]
type item = Mirror of mirror | Splitter of splitter [@@deriving sexp]
type t = item Map2d.t [@@deriving sexp]

let parse =
  let open Angstrom in
  let symbol =
    choice
      [
        char '.' *> return None;
        char '/' *> return (Some (Mirror NE));
        char '\\' *> return (Some (Mirror NW));
        char '|' *> return (Some (Splitter NS));
        char '-' *> return (Some (Splitter EW));
      ]
  in
  parse (Map2d.parse symbol)

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((0 1) (Splitter NS)) ((1 0) (Splitter NS)) ((1 7) (Splitter EW))
     ((1 8) (Splitter NS)) ((2 1) (Splitter EW)) ((2 9) (Mirror NE))
     ((3 7) (Splitter EW)) ((3 9) (Mirror NE)) ((4 1) (Mirror NW))
     ((4 6) (Mirror NE)) ((4 7) (Mirror NE)) ((5 0) (Mirror NW))
     ((5 2) (Splitter NS)) ((5 9) (Splitter NS)) ((6 2) (Splitter EW))
     ((6 6) (Mirror NW)) ((6 8) (Splitter EW)) ((7 6) (Mirror NW))
     ((7 7) (Splitter NS)) ((7 8) (Splitter NS)) ((8 3) (Splitter NS))
     ((9 5) (Mirror NW)) ((9 8) (Mirror NW))) |}]

type dir = N | S | E | W [@@deriving compare, sexp]

let shift (i, j) = function
  | E -> (i + 1, j)
  | W -> (i - 1, j)
  | N -> (i, j - 1)
  | S -> (i, j + 1)

let reflect_dir md d =
  match (md, d) with
  | NW, E -> S
  | NW, N -> W
  | NW, W -> N
  | NE, E -> N
  | NE, N -> E
  | NW, S -> E
  | NE, S -> W
  | NE, W -> S

let split sd d =
  match (sd, d) with
  | NS, (E | W) -> Some (N, S)
  | NS, (N | S) -> None
  | EW, (N | S) -> Some (E, W)
  | EW, (E | W) -> None

module State = struct
  module T = struct
    type t = { pos : Pos.t; dir : dir } [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let next m { State.pos; dir } =
  let dirs =
    match Map.find m pos with
    | None -> [ dir ]
    | Some (Mirror md) -> [ reflect_dir md dir ]
    | Some (Splitter sd) -> (
        match split sd dir with None -> [ dir ] | Some (d1, d2) -> [ d1; d2 ])
  in
  let go dir = { State.pos = shift pos dir; dir } in
  List.map ~f:go dirs

let start_p1 = { State.pos = (0, 0); dir = E }

let energize start m =
  let bounds = Map2d.bounds m in
  let q : State.t Queue.t = Queue.create () in
  Queue.enqueue q start;
  let visited = ref (Set.empty (module State)) in
  while not (Queue.is_empty q) do
    let s = Queue.dequeue_exn q in
    if Map2d.in_bounds bounds s.pos then
      if Set.mem !visited s then ()
      else (
        visited := Set.add !visited s;
        Queue.enqueue_all q (next m s))
    else ()
  done;
  Set.to_list !visited
  |> List.map ~f:(fun s -> s.pos)
  |> Set.of_list (module Pos)

let view s =
  let m =
    Set.to_list s
    |> List.map ~f:(fun k -> (k, ()))
    |> Map.of_alist_exn (module Pos)
  in
  Map2d.view m (fun () -> "#")

let%expect_test "energize" =
  parse sample |> energize start_p1 |> view;
  [%expect
    {|
    ######....
    .#...#....
    .#...#####
    .#...##...
    .#...##...
    .#...##...
    .#..####..
    ########..
    .#######..
    .#...#.#.. |}]

let result m = energize start_p1 m |> Set.length

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 46 |}]

let initial_states m =
  let { Map2d.imax; jmax; _ } = Map2d.bounds m in
  (List.range 0 imax ~stop:`inclusive
  |> List.concat_map ~f:(fun i ->
         [ { State.pos = (i, 0); dir = S }; { pos = (i, jmax); dir = N } ]))
  @ (List.range 0 jmax ~stop:`inclusive
    |> List.concat_map ~f:(fun j ->
           [ { State.pos = (0, j); dir = E }; { pos = (imax, j); dir = W } ]))

let result2 m =
  initial_states m
  |> List.map ~f:(fun s -> energize s m |> Set.length)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 51 |}]

let run () = main All parse result result2
