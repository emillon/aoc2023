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

let wrap_pos bounds (i, j) =
  let { Map2d.imax; jmax; _ } = bounds in
  (* XXX fix that at parse time *)
  let imax = imax + 2 in
  let jmax = jmax + 2 in
  let i = i % imax in
  let j = j % jmax in
  (i, j)

let mem_infinite m bounds p = Map2d.mem m (wrap_pos bounds p)

let%expect_test "mem_infinite" =
  let t = parse sample in
  for j = -16 to 16 do
    for i = -16 to 16 do
      if mem_infinite t.map t.bounds (i, j) then printf "#" else printf "."
    done;
    printf "\n"
  done;
  [%expect
    {|
    ...#..##..#...#..##..#...#..##..#
    .##.........##.........##........
    ####..##.#.####..##.#.####..##.#.
    #.##..##..##.##..##..##.##..##..#
    .................................
    .................................
    ##.#......###.#......###.#......#
    #..#..###.##..#..###.##..#..###.#
    ..#....#.#...#....#.#...#....#.#.
    #........#.#........#.#........#.
    ####..##...####..##...####..##...
    ...#..##..#...#..##..#...#..##..#
    .##.........##.........##........
    ####..##.#.####..##.#.####..##.#.
    #.##..##..##.##..##..##.##..##..#
    .................................
    .................................
    ##.#......###.#......###.#......#
    #..#..###.##..#..###.##..#..###.#
    ..#....#.#...#....#.#...#....#.#.
    #........#.#........#.#........#.
    ####..##...####..##...####..##...
    ...#..##..#...#..##..#...#..##..#
    .##.........##.........##........
    ####..##.#.####..##.#.####..##.#.
    #.##..##..##.##..##..##.##..##..#
    .................................
    .................................
    ##.#......###.#......###.#......#
    #..#..###.##..#..###.##..#..###.#
    ..#....#.#...#....#.#...#....#.#.
    #........#.#........#.#........#.
    ####..##...####..##...####..##... |}]

let blocked ~infinite m bounds p =
  if infinite then mem_infinite m bounds p
  else if Map2d.in_bounds bounds p then Map2d.mem m p
  else true

let step ~infinite m bounds start =
  Set.fold start
    ~init:(Set.empty (module Pos))
    ~f:(fun s pos ->
      all_dirs
      |> List.fold ~init:s ~f:(fun acc d ->
             let new_pos = shift pos d in
             if blocked ~infinite m bounds new_pos then acc
             else Set.add acc new_pos))

let%expect_test "step" =
  let t = parse sample in
  let start = Set.singleton (module Pos) t.start in
  let s = ref start in
  let go () =
    s := step ~infinite:false t.map t.bounds !s;
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

let result_gen t ~n ~infinite =
  iterate_n
    (Set.singleton (module Pos) t.start)
    ~f:(step ~infinite t.map t.bounds)
    ~n
  |> Set.length

let result t = result_gen t ~n:64 ~infinite:false

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 29 |}]

let interpolate v0 v1 v2 =
  let a0 = v1 - v0 in
  let a1 = v2 - v1 in
  let b0 = a1 - a0 in
  `Staged (fun n -> v0 + (n * a0) + (b0 * (n * (n - 1) / 2)))

let result2 t =
  (* XXX fix *)
  let sz = t.bounds.imax + 2 in
  let big_n = 26501365 in
  let modulo = big_n % sz in
  let bigstep start =
    iterate_n start ~f:(step ~infinite:true t.map t.bounds) ~n:sz
  in
  let start =
    iterate_n
      (Set.singleton (module Pos) t.start)
      ~f:(step ~infinite:true t.map t.bounds)
      ~n:modulo
  in
  let s0 = start in
  let s1 = bigstep s0 in
  let s2 = bigstep s1 in
  let (`Staged f) =
    interpolate (Set.length s0) (Set.length s1) (Set.length s2)
  in
  f (big_n / sz)

let run () = main All parse result result2
