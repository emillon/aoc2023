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

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type t = Set.M(Pos).t [@@deriving sexp]

let parse s =
  let symbol =
    let open Angstrom in
    let+ pos
    and+ symbol =
      choice [ char '.' *> return false; char '#' *> return true ]
    in
    (symbol, pos)
  in
  let line =
    let open Angstrom in
    let+ s = many1 symbol <* end_of_line in
    ( List.filter_map s ~f:(fun (ok, pos) -> Option.some_if ok pos),
      List.length s )
  in
  let map =
    let open Angstrom in
    let+ ll = many1 line in
    let width = (List.hd_exn ll |> snd) + 1 in
    let off_to_pos off = (off % width, off / width) in
    List.concat_map ~f:fst ll
    |> List.map ~f:(fun off -> off_to_pos off)
    |> Set.of_list (module Pos)
  in
  Angstrom.parse_string ~consume:All map s |> Result.ok_or_failwith

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect {| ((0 2) (0 9) (1 5) (3 0) (4 9) (6 4) (7 1) (7 8) (9 6)) |}]

let bounds =
  Set.fold ~init:(Int.min_value, Int.min_value) ~f:(fun (i, j) (max_i, max_j) ->
      (Int.max i max_i, Int.max j max_j))

let empty_columns t =
  let imax, jmax = bounds t in
  let r = ref [] in
  let is_column_empty i =
    let ok = ref true in
    for j = 0 to jmax do
      if Set.mem t (i, j) then ok := false
    done;
    !ok
  in
  for i = 0 to imax do
    if is_column_empty i then r := i :: !r
  done;
  !r

let empty_rows t =
  let imax, jmax = bounds t in
  let r = ref [] in
  let is_row_empty j =
    let ok = ref true in
    for i = 0 to imax do
      if Set.mem t (i, j) then ok := false
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

let add_row t row =
  Set.map
    (module Pos)
    t
    ~f:(fun (i, j) ->
      (* XXX off by one? *)
      let new_j = if j > row then j + 1 else j in
      (i, new_j))

let add_empty_rows rows t = List.fold rows ~init:t ~f:add_row

let add_column t column =
  Set.map
    (module Pos)
    t
    ~f:(fun (i, j) ->
      (* XXX off by one? *)
      let new_i = if i > column then i + 1 else i in
      (new_i, j))

let add_empty_columns columns t = List.fold columns ~init:t ~f:add_column

let expand t =
  let empty_rows = empty_rows t in
  let empty_columns = empty_columns t in
  t |> add_empty_rows empty_rows |> add_empty_columns empty_columns

let view t =
  let imax, jmax = bounds t in
  for j = 0 to jmax do
    for i = 0 to imax do
      if Set.mem t (i, j) then printf "#" else printf "."
    done;
    printf "\n"
  done

let%expect_test "expand" =
  parse sample |> expand |> view;
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
  Set.fold t ~init ~f:(fun acc a ->
      let _, gt = Set.split_le_gt t a in
      Set.fold gt ~init:acc ~f:(fun acc b -> f acc a b))

let result t =
  expand t
  |> fold_over_unique_pairs ~init:0 ~f:(fun acc a b -> acc + distance a b)

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 374 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_all path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_all path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
