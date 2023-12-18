open Base
open! Lib
open Stdio

let sample =
  [
    "...#......";
    ".......#..";
    "#.........";
    "..........";
    "......#...";
    ".#........";
    ".........#";
    "..........";
    ".......#..";
    "#...#.....";
  ]
  |> String.concat_lines

type t = unit Map2d.t [@@deriving sexp]

let parse =
  let open Angstrom in
  let symbol =
    choice [ char '.' *> return None; char '#' *> return (Some ()) ]
  in
  parse (Map2d.parse symbol)

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((0 2) ()) ((0 9) ()) ((1 5) ()) ((3 0) ()) ((4 9) ()) ((6 4) ()) ((7 1) ())
     ((7 8) ()) ((9 6) ())) |}]

let empty_columns t =
  let { Map2d.imax; jmax; _ } = Map2d.bounds t in
  let r = ref [] in
  let is_column_empty i =
    let ok = ref true in
    for j = 0 to jmax do
      if Map.mem t (i, j) then ok := false
    done;
    !ok
  in
  for i = 0 to imax do
    if is_column_empty i then r := i :: !r
  done;
  !r

let empty_rows t =
  let { Map2d.imax; jmax; _ } = Map2d.bounds t in
  let r = ref [] in
  let is_row_empty j =
    let ok = ref true in
    for i = 0 to imax do
      if Map.mem t (i, j) then ok := false
    done;
    !ok
  in
  for j = 0 to jmax do
    if is_row_empty j then r := j :: !r
  done;
  !r

let%expect_test "is_empty_column" =
  parse sample |> empty_columns |> [%sexp_of: int list] |> print_s;
  [%expect {| (8 5 2) |}]

let%expect_test "is_empty_row" =
  parse sample |> empty_rows |> [%sexp_of: int list] |> print_s;
  [%expect {| (7 3) |}]

let add_row ~factor t row =
  Map.map_keys_exn
    (module Pos)
    t
    ~f:(fun (i, j) ->
      let new_j = if j > row then j + factor - 1 else j in
      (i, new_j))

let add_empty_rows ~factor rows t = List.fold rows ~init:t ~f:(add_row ~factor)

let add_column ~factor t column =
  Map.map_keys_exn
    (module Pos)
    t
    ~f:(fun (i, j) ->
      let new_i = if i > column then i + factor - 1 else i in
      (new_i, j))

let add_empty_columns ~factor columns t =
  List.fold columns ~init:t ~f:(add_column ~factor)

let expand ~factor t =
  let empty_rows = empty_rows t in
  let empty_columns = empty_columns t in
  t
  |> add_empty_rows ~factor empty_rows
  |> add_empty_columns ~factor empty_columns

let view t = Map2d.view t (fun () -> "#")

let%expect_test "expand" =
  parse sample |> expand ~factor:2 |> view;
  [%expect
    {|
    ....#........
    .........#...
    #............
    .............
    .............
    ........#....
    .#...........
    ............#
    .............
    .............
    .........#...
    #....#....... |}]

let distance (ai, aj) (bi, bj) = abs (ai - bi) + abs (aj - bj)

let%expect_test "distance" =
  let test a b = distance a b |> printf "%d\n" in
  test (2, 3) (2, 4);
  [%expect "1"];
  test (2, 3) (3, 4);
  [%expect "2"];
  test (1, 6) (5, 11);
  [%expect "9"]

let fold_over_unique_pairs t ~init ~f =
  Map.fold t ~init ~f:(fun ~key:a ~data:() acc ->
      let _, gt = Map.split_le_gt t a in
      Map.fold gt ~init:acc ~f:(fun ~key:b ~data:() acc -> f acc a b))

let sum_of_distances t =
  fold_over_unique_pairs t ~init:0 ~f:(fun acc a b -> acc + distance a b)

let result_gen t ~factor = expand ~factor t |> sum_of_distances
let result t = result_gen t ~factor:2

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 374 |}]

let result2 t = expand ~factor:1_000_000 t |> sum_of_distances

let%expect_test "result2" =
  let test factor = parse sample |> result_gen ~factor |> printf "%d\n" in
  test 10;
  [%expect {| 1030 |}];
  test 100;
  [%expect {| 8410 |}]

let run () = main All parse result result2
