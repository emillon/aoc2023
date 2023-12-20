open Base
open! Lib
open Stdio

let sample =
  [
    "O....#....";
    "O.OO#....#";
    ".....##...";
    "OO.#O....O";
    ".O.....O#.";
    "O.#..O.#.#";
    "..O..#O..O";
    ".......O..";
    "#....###..";
    "#OO..#....";
  ]
  |> String.concat_lines

type rock = Cube | Rock [@@deriving compare, equal, sexp]
type t = rock Map2d.t [@@deriving compare, equal, sexp]

let parse =
  let open Angstrom in
  let symbol =
    choice
      [
        char '.' *> return None;
        char '#' *> return (Some Cube);
        char 'O' *> return (Some Rock);
      ]
  in
  parse (Map2d.parse symbol)

let view t = Map2d.view t (function Cube -> "#" | Rock -> "O")

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((0 0) Rock) ((0 1) Rock) ((0 3) Rock) ((0 5) Rock) ((0 8) Cube)
     ((0 9) Cube) ((1 3) Rock) ((1 4) Rock) ((1 9) Rock) ((2 1) Rock)
     ((2 5) Cube) ((2 6) Rock) ((2 9) Rock) ((3 1) Rock) ((3 3) Cube)
     ((4 1) Cube) ((4 3) Rock) ((5 0) Cube) ((5 2) Cube) ((5 5) Rock)
     ((5 6) Cube) ((5 8) Cube) ((5 9) Cube) ((6 2) Cube) ((6 6) Rock)
     ((6 8) Cube) ((7 4) Rock) ((7 5) Cube) ((7 7) Rock) ((7 8) Cube)
     ((8 4) Cube) ((9 1) Cube) ((9 3) Rock) ((9 5) Cube) ((9 6) Rock)) |}];
  parse sample |> view;
  [%expect
    {|
    O....#....
    O.OO#....#
    .....##...
    OO.#O....O
    .O.....O#.
    O.#..O.#.#
    ..O..#O..O
    .......O..
    #....###..
    #OO..#.... |}]

type dir = N | S | E | W

let shift (i, j) = function
  | N -> (i, j - 1)
  | S -> (i, j + 1)
  | W -> (i - 1, j)
  | E -> (i + 1, j)

let rec next_available_in_dir t bounds p dir =
  let dst = shift p dir in
  if Map2d.in_bounds bounds dst then
    if Map.mem t dst then p else next_available_in_dir t bounds dst dir
  else p

let step dir bounds t =
  Map.map_keys_exn
    (module Pos)
    t
    ~f:(fun p ->
      match Map.find_exn t p with
      | Cube -> p
      | Rock -> next_available_in_dir t bounds p dir)

let%expect_test "step" =
  let t = parse sample in
  t |> step N (Map2d.bounds t) |> view;
  [%expect
    {|
    OOOO.#.O..
    O...#....#
    O...O##..O
    ...#.OO...
    OO......#.
    .O#....#.#
    ..O..#.O.O
    ..O.......
    #....###..
    #....#.... |}]

let rec fixpoint ~equal ~f x =
  let y = f x in
  if equal x y then x else fixpoint ~equal ~f y

let move_all dir t =
  let bounds = Map2d.bounds t in
  fixpoint ~f:(step dir bounds) ~equal:[%equal: t] t

let%expect_test "move_all" =
  parse sample |> move_all N |> view;
  [%expect
    {|
    OOOO.#.O..
    OO..#....#
    OO..O##..O
    O..#.OO...
    ........#.
    ..#....#.#
    ..O..#.O.O
    ..O.......
    #....###..
    #....#.... |}]

let load bounds t =
  let { Map2d.jmax; _ } = bounds in
  let score_for_row j = jmax - j + 1 in
  Map.fold t ~init:0 ~f:(fun ~key:(_i, j) ~data acc ->
      match data with Cube -> acc | Rock -> acc + score_for_row j)

let result t =
  let bounds = Map2d.bounds t in
  let moved = move_all N t in
  load bounds moved

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 136 |}]

let one_cycle t = t |> move_all N |> move_all W |> move_all S |> move_all E

let%expect_test "one_cycle" =
  let r = ref (parse sample) in
  let go () =
    r := one_cycle !r;
    view !r
  in
  go ();
  [%expect
    {|
    .....#....
    ....#...O#
    ...OO##...
    .OO#......
    .....OOO#.
    .O#...O#.#
    ....O#....
    ......OOOO
    #...O###..
    #..OO#.... |}];
  go ();
  [%expect
    {|
    .....#....
    ....#...O#
    .....##...
    ..O#......
    .....OOO#.
    .O#...O#.#
    ....O#...O
    .......OOO
    #..OO###..
    #.OOO#...O |}];
  go ();
  [%expect
    {|
    .....#....
    ....#...O#
    .....##...
    ..O#......
    .....OOO#.
    .O#...O#.#
    ....O#...O
    .......OOO
    #...O###.O
    #.OOO#...O |}]

module State = struct
  module T = struct
    type nonrec t = t [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let cycle_values t0 =
  let rec go t m n =
    match Map.find m t with
    | Some old_n -> (old_n, m)
    | None ->
        let new_m = Map.add_exn m ~key:t ~data:n in
        go (one_cycle t) new_m (n + 1)
  in
  go t0 (Map.empty (module State)) 0

let%expect_test "cyclic_values" =
  let n, m = parse sample |> cycle_values in
  (Map.length m, n) |> [%sexp_of: int * int] |> print_s;
  [%expect {| (10 3) |}]

let result2 t =
  let bounds = Map2d.bounds t in
  let n, values = cycle_values t in
  let values_len = Map.length values in
  let modulus = values_len - n in
  let modulo = (1000000000 - n) % modulus in
  let bucket = n + modulo in
  Map.to_alist values
  |> List.find_exn ~f:(fun (_v, k) -> k = bucket)
  |> fst |> load bounds

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 64 |}]

let run () = main All parse result result2
