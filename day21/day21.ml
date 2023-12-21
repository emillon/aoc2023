open Base
open! Lib
open Stdio

let sample =
  [
    "...........";
    ".....###.#.";
    ".###.##..#.";
    "..#.#...#..";
    "....#.#....";
    ".##..S####.";
    ".##..#...#.";
    ".......##..";
    ".##.#.####.";
    ".##..##.##.";
    "...........";
  ]
  |> String.concat_lines

type t = { start : Pos.t; map : unit Map2d.t; bounds : Map2d.bounds }
[@@deriving sexp]

let parse =
  let open Angstrom in
  let symbol =
    choice
      [
        char '#' *> return (Some `Wall);
        char 'S' *> return (Some `Start);
        char '.' *> return None;
      ]
  in
  let t =
    let+ m = Map2d.parse symbol in
    let start = ref None in
    let map =
      Map.filter_mapi m ~f:(fun ~key ~data ->
          match data with
          | `Wall -> Some ()
          | `Start ->
              start := Some key;
              None)
    in
    let bounds = Map2d.bounds map in
    { start = Option.value_exn !start; map; bounds }
  in
  parse t

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    ((start (5 5))
     (map
      (((1 2) ()) ((1 5) ()) ((1 6) ()) ((1 8) ()) ((1 9) ()) ((2 2) ())
       ((2 3) ()) ((2 5) ()) ((2 6) ()) ((2 8) ()) ((2 9) ()) ((3 2) ())
       ((4 3) ()) ((4 4) ()) ((4 8) ()) ((5 1) ()) ((5 2) ()) ((5 6) ())
       ((5 9) ()) ((6 1) ()) ((6 2) ()) ((6 4) ()) ((6 5) ()) ((6 8) ())
       ((6 9) ()) ((7 1) ()) ((7 5) ()) ((7 7) ()) ((7 8) ()) ((8 3) ())
       ((8 5) ()) ((8 7) ()) ((8 8) ()) ((8 9) ()) ((9 1) ()) ((9 2) ())
       ((9 5) ()) ((9 6) ()) ((9 8) ()) ((9 9) ())))
     (bounds ((imin 1) (imax 9) (jmin 1) (jmax 9)))) |}]

type dir = N | S | E | W

let all_dirs = [ N; S; E; W ]

let shift (i, j) = function
  | E -> (i + 1, j)
  | W -> (i - 1, j)
  | N -> (i, j - 1)
  | S -> (i, j + 1)

let step m bounds start =
  Set.fold start
    ~init:(Set.empty (module Pos))
    ~f:(fun s pos ->
      all_dirs
      |> List.fold ~init:s ~f:(fun acc d ->
             let new_pos = shift pos d in
             if Map2d.in_bounds bounds new_pos then
               if Map2d.mem m new_pos then acc else Set.add acc new_pos
             else acc))

let%expect_test "step" =
  let t = parse sample in
  let start = Set.singleton (module Pos) t.start in
  let s = ref start in
  let go () =
    s := step t.map t.bounds !s;
    Map2d.view ~sets:[ (!s, 'O'); (start, 'S') ] t.map (fun () -> "#")
  in
  go ();
  [%expect
    {|
    ..........
    .....###.#
    .###.##..#
    ..#.#...#.
    ....#O#...
    .##.OS####
    .##..#...#
    .......##.
    .##.#.####
    .##..##.## |}];
  go ();
  [%expect
    {|
    ..........
    .....###.#
    .###.##..#
    ..#.#O..#.
    ....#.#...
    .##O.O####
    .##.O#...#
    .......##.
    .##.#.####
    .##..##.## |}];
  go ();
  [%expect
    {|
    ..........
    .....###.#
    .###.##..#
    ..#.#.O.#.
    ...O#O#...
    .##.OS####
    .##O.#...#
    ....O..##.
    .##.#.####
    .##..##.## |}]

let rec iterate_n x ~f ~n = if n = 0 then x else iterate_n (f x) ~f ~n:(n - 1)

let result t =
  iterate_n (Set.singleton (module Pos) t.start) ~f:(step t.map t.bounds) ~n:64
  |> Set.length

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 29 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
