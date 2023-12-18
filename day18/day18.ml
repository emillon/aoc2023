open Base
open! Lib
open Stdio

let sample =
  [
    "R 6 (#70c710)";
    "D 5 (#0dc571)";
    "L 2 (#5713f0)";
    "D 2 (#d2c081)";
    "R 2 (#59c680)";
    "D 2 (#411b91)";
    "L 5 (#8ceee2)";
    "U 2 (#caa173)";
    "L 1 (#1b58a2)";
    "U 2 (#caa171)";
    "R 2 (#7807d2)";
    "U 3 (#a77fa3)";
    "L 2 (#015232)";
    "U 2 (#7a21e3)";
  ]
  |> String.concat_lines

type dir = N | S | E | W [@@deriving sexp]

let shift (i, j) = function
  | E -> (i + 1, j)
  | W -> (i - 1, j)
  | N -> (i, j - 1)
  | S -> (i, j + 1)

type line = { dir : dir; n : int; color : string } [@@deriving sexp]
type t = line list [@@deriving sexp]

let parse =
  let open Angstrom in
  let dir =
    choice
      [
        char 'U' *> return N;
        char 'D' *> return S;
        char 'L' *> return W;
        char 'R' *> return E;
      ]
  in
  let line =
    let+ dir = dir <* char ' '
    and+ n = number <* string " (#"
    and+ color = take_while Char.is_hex_digit <* char ')' <* end_of_line in
    { dir; n; color }
  in
  let input = many1 line in
  parse input

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((dir E) (n 6) (color 70c710)) ((dir S) (n 5) (color 0dc571))
     ((dir W) (n 2) (color 5713f0)) ((dir S) (n 2) (color d2c081))
     ((dir E) (n 2) (color 59c680)) ((dir S) (n 2) (color 411b91))
     ((dir W) (n 5) (color 8ceee2)) ((dir N) (n 2) (color caa173))
     ((dir W) (n 1) (color 1b58a2)) ((dir N) (n 2) (color caa171))
     ((dir E) (n 2) (color 7807d2)) ((dir N) (n 3) (color a77fa3))
     ((dir W) (n 2) (color 015232)) ((dir N) (n 2) (color 7a21e3))) |}]

let rec add_line (m, pos) dir n =
  if n = 0 then (m, pos)
  else
    let new_pos = shift pos dir in
    let new_m = Map.add_exn m ~key:new_pos ~data:() in
    add_line (new_m, new_pos) dir (n - 1)

let build_map t =
  let empty_map = Map.empty (module Pos) in
  List.fold t
    ~init:(empty_map, (0, 0))
    ~f:(fun acc { dir; n; _ } -> add_line acc dir n)
  |> fst

let%expect_test "build_map" =
  let m = parse sample |> build_map in
  Map2d.view m (fun () -> "#");
  [%expect
    {|
    #######
    #.....#
    ###...#
    ..#...#
    ..#...#
    ###.###
    #...#..
    ##..###
    .#....#
    .###### |}]

let exterior_start m =
  let { Map2d.imax; jmax; imin; jmin } = Map2d.bounds m in
  (List.range imin imax ~stop:`inclusive
  |> List.concat_map ~f:(fun i -> [ (i, jmin); (i, jmax) ]))
  @ (List.range jmin jmax ~stop:`inclusive
    |> List.concat_map ~f:(fun j -> [ (imin, j); (imax, j) ]))
  |> Set.of_list (module Pos)
  |> Set.filter ~f:(fun p -> not (Map.mem m p))

let all_dirs = [ N; S; E; W ]

let fill_from m start =
  let visited = ref (Set.empty (module Pos)) in
  let bounds = Map2d.bounds m in
  let q = Queue.of_list (Set.to_list start) in
  while not (Queue.is_empty q) do
    let p = Queue.dequeue_exn q in
    all_dirs
    |> List.map ~f:(shift p)
    |> List.iter ~f:(fun new_pos ->
           if Map.mem m new_pos then ()
           else if Set.mem !visited new_pos then ()
           else if Map2d.in_bounds ~from_min:true bounds new_pos then (
             visited := Set.add !visited new_pos;
             Queue.enqueue q new_pos)
           else ())
  done;
  !visited

let%expect_test "exterior_start" =
  let m = parse sample |> build_map in
  let start = exterior_start m in
  let filled = fill_from m start in
  Map2d.view m (fun () -> "#") ~sets:[ (start, 's'); (filled, 'f') ];
  [%expect
    {|
    #######
    #.....#
    ###...#
    sf#...#
    sf#...#
    ###.###
    #...#fs
    ##..###
    s#....#
    s###### |}]

let bounds_size { Map2d.imax; jmax; imin; jmin } =
  (imax - imin + 1) * (jmax - jmin + 1)

let result t =
  let m = build_map t in
  let start = exterior_start m in
  let filled = fill_from m start in
  let bounds = Map2d.bounds m in
  bounds_size bounds - Set.length filled

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {|
    62 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
