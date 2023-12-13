open Base
open! Lib
open Stdio

let sample =
  [
    "#.##..##.";
    "..#.##.#.";
    "##......#";
    "##......#";
    "..#.##.#.";
    "..##..##.";
    "#.#.##.#.";
    "";
    "#...##..#";
    "#....#..#";
    "..##..###";
    "#####.##.";
    "#####.##.";
    "..##..###";
    "#....#..#";
  ]
  |> String.concat_lines

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type map = Set.M(Pos).t [@@deriving sexp]
type t = map list [@@deriving sexp]

let parse =
  let open Angstrom in
  let symbol =
    let+ pos
    and+ symbol =
      choice [ char '.' *> return false; char '#' *> return true ]
    in
    (symbol, pos)
  in
  let line =
    let+ s = many1 symbol <* end_of_line in
    ( List.filter_map s ~f:(fun (ok, pos) -> Option.some_if ok pos),
      List.length s )
  in
  let map =
    let+ pos_start = pos and+ ll = many1 line in
    let width = (List.hd_exn ll |> snd) + 1 in
    let off_to_pos off =
      let off = off - pos_start in
      (off % width, off / width)
    in
    List.concat_map ~f:fst ll
    |> List.map ~f:(fun off -> off_to_pos off)
    |> Set.of_list (module Pos)
  in
  let input = sep_by end_of_line map in
  parse input

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((0 0) (0 2) (0 3) (0 6) (1 2) (1 3) (2 0) (2 1) (2 4) (2 5) (2 6) (3 0)
      (3 5) (4 1) (4 4) (4 6) (5 1) (5 4) (5 6) (6 0) (6 5) (7 0) (7 1) (7 4)
      (7 5) (7 6) (8 2) (8 3))
     ((0 0) (0 1) (0 3) (0 4) (0 6) (1 3) (1 4) (2 2) (2 3) (2 4) (2 5) (3 2)
      (3 3) (3 4) (3 5) (4 0) (4 3) (4 4) (5 0) (5 1) (5 6) (6 2) (6 3) (6 4)
      (6 5) (7 2) (7 3) (7 4) (7 5) (8 0) (8 1) (8 2) (8 5) (8 6))) |}]

let bounds =
  Set.fold ~init:(Int.max_value, Int.max_value, Int.min_value, Int.min_value)
    ~f:(fun (min_i, min_j, max_i, max_j) (i, j) ->
      (Int.min i min_i, Int.min j min_j, Int.max i max_i, Int.max j max_j))

type reflection = Col of int | Row of int [@@deriving sexp]

let reflect_col (i, j) col_i =
  let new_i = (2 * col_i) - i + 1 in
  (new_i, j)

let%expect_test "reflect_col" =
  let test i = reflect_col (i, 0) 3 |> [%sexp_of: Pos.t] |> print_s in
  test 2;
  [%expect {| (5 0) |}];
  test 3;
  [%expect {| (4 0) |}];
  test 4;
  [%expect {| (3 0) |}];
  test 5;
  [%expect {| (2 0) |}];
  test 6;
  [%expect {| (1 0) |}]

let view m =
  let imin, jmin, imax, jmax = bounds m in
  for j = jmin to jmax do
    for i = imin to imax do
      if Set.mem m (i, j) then printf "#" else printf "."
    done;
    printf "\n"
  done

let transpose = Set.map (module Pos) ~f:(fun (i, j) -> (j, i))

type equal_kind = Strict | Smudge

let equal kind a b =
  match kind with
  | Strict -> Set.equal a b
  | Smudge ->
      let diff = Set.symmetric_diff a b in
      Sequence.length diff = 1

let is_col_reflection kind m imax col_i =
  (* i means i/i+1 act as mirror *)
  let left, right = Set.partition_tf m ~f:(fun (ri, _rj) -> ri <= col_i) in
  let part_to_mirror, side, min, max =
    if 2 * col_i < imax - 1 then (right, left, col_i + 1, (2 * col_i) + 1)
    else (left, right, (2 * col_i) - imax + 1, col_i)
  in
  let mirror =
    Set.filter_map
      (module Pos)
      part_to_mirror
      ~f:(fun (i, j) ->
        if min <= i && i <= max then Some (reflect_col (i, j) col_i) else None)
  in
  equal kind mirror side

let find_col_reflection kind m =
  let _, _, imax, _ = bounds m in
  List.range 0 imax |> List.find ~f:(is_col_reflection kind m imax)

let find_row_reflection kind m = m |> transpose |> find_col_reflection kind

let find_reflection kind m =
  match find_col_reflection kind m with
  | Some n -> Some (Col n)
  | None ->
      let%map.Option n = find_row_reflection kind m in
      Row n

let%expect_test "find_reflection" =
  parse sample
  |> List.map ~f:(find_reflection Strict)
  |> [%sexp_of: reflection option list] |> print_s;
  [%expect {| (((Col 4)) ((Row 3))) |}]

let score = function Col n -> n + 1 | Row n -> 100 * (n + 1)

let result_gen l kind =
  l
  |> List.map ~f:(fun m -> find_reflection kind m |> Option.value_exn |> score)
  |> sum

let result l = result_gen l Strict

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 405 |}]

let result2 l = result_gen l Smudge

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 400 |}]

let run () = main All parse result result2
