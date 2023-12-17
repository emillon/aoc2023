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

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type mirror = NW | NE [@@deriving sexp]
type splitter = NS | EW [@@deriving sexp]
type item = Mirror of mirror | Splitter of splitter [@@deriving sexp]
type t = item Map.M(Pos).t [@@deriving sexp]

let parse =
  let open Angstrom in
  let symbol =
    let+ pos
    and+ symbol =
      choice
        [
          char '.' *> return None;
          char '/' *> return (Some (Mirror NE));
          char '\\' *> return (Some (Mirror NW));
          char '|' *> return (Some (Splitter NS));
          char '-' *> return (Some (Splitter EW));
        ]
    in
    (symbol, pos)
  in
  let line =
    let+ s = many1 symbol <* end_of_line in
    ( List.filter_map s ~f:(fun (sym_opt, pos) ->
          Option.map sym_opt ~f:(fun sym -> (sym, pos))),
      List.length s )
  in
  let map =
    let+ ll = many1 line in
    let width = (List.hd_exn ll |> snd) + 1 in
    let off_to_pos off = (off % width, off / width) in
    List.concat_map ~f:fst ll
    |> List.map ~f:(fun (item, off) -> (off_to_pos off, item))
    |> Map.of_alist_exn (module Pos)
  in
  parse map

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

let bounds =
  Map.fold ~init:(Int.min_value, Int.min_value)
    ~f:(fun ~key:(i, j) ~data:_ (max_i, max_j) ->
      (Int.max i max_i, Int.max j max_j))

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

let in_bounds (imax, jmax) (i, j) = 0 <= i && i <= imax && 0 <= j && j <= jmax

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

let energize m =
  let bounds = bounds m in
  let q = Queue.create () in
  Queue.enqueue q { State.pos = (0, 0); dir = E };
  let visited = ref (Set.empty (module State)) in
  while not (Queue.is_empty q) do
    let s = Queue.dequeue_exn q in
    if in_bounds bounds s.pos then
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
  let imax, jmax =
    bounds
      (Set.to_list s
      |> List.map ~f:(fun k -> (k, ()))
      |> Map.of_alist_exn (module Pos))
  in
  for j = 0 to jmax do
    for i = 0 to imax do
      if Set.mem s (i, j) then printf "#" else printf "."
    done;
    printf "\n"
  done

let%expect_test "energize" =
  parse sample |> energize |> view;
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

let result m = energize m |> Set.length

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 46 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
