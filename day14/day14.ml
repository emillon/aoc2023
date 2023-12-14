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

type rock = Cube | Rock [@@deriving equal, sexp]
type t = rock Map.M(Pos).t [@@deriving equal, sexp]

let parse =
  let open Angstrom in
  let symbol =
    let+ pos
    and+ symbol =
      choice
        [
          char '.' *> return None;
          char '#' *> return (Some Cube);
          char 'O' *> return (Some Rock);
        ]
    in
    Option.map symbol ~f:(fun s -> (s, pos))
  in
  let line =
    let+ s = many1 symbol <* end_of_line in
    (List.filter_opt s, List.length s)
  in
  let map =
    let+ ll = many1 line in
    let width = (List.hd_exn ll |> snd) + 1 in
    let off_to_pos off = (off % width, off / width) in
    List.concat_map ~f:fst ll
    |> List.map ~f:(fun (sym, off) -> (off_to_pos off, sym))
    |> Map.of_alist_exn (module Pos)
  in
  parse map

let bounds =
  Map.fold ~init:(Int.min_value, Int.min_value)
    ~f:(fun ~key:(i, j) ~data:_ (max_i, max_j) ->
      (Int.max i max_i, Int.max j max_j))

let view t =
  let imax, jmax = bounds t in
  for j = 0 to jmax do
    for i = 0 to imax do
      match Map.find t (i, j) with
      | Some Cube -> printf "#"
      | Some Rock -> printf "O"
      | None -> printf "."
    done;
    printf "\n"
  done

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

let step t =
  Map.map_keys_exn
    (module Pos)
    t
    ~f:(fun (i, j) ->
      if j = 0 then (i, j)
      else
        match Map.find t (i, j) with
        | None | Some Cube -> (i, j)
        | Some Rock ->
            let north = (i, j - 1) in
            if Map.mem t north then (i, j) else north)

let%expect_test "step" =
  parse sample |> step |> view;
  [%expect
    {|
    O.OO.#....
    O...#....#
    OO..O##..O
    ...#...O..
    OO...O..#.
    ..#...O#.#
    ..O..#.O.O
    ..........
    #OO..###..
    #....#.... |}]

let rec fixpoint ~equal ~f x =
  let y = f x in
  if equal x y then x else fixpoint ~equal ~f y

let move_all = fixpoint ~f:step ~equal:[%equal: t]

let%expect_test "move_all" =
  parse sample |> move_all |> view;
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

let result t =
  let _imax, jmax = bounds t in
  let moved = move_all t in
  let score_for_row j = jmax - j + 1 in
  Map.fold moved ~init:0 ~f:(fun ~key:(_i, j) ~data acc ->
      match data with Cube -> acc | Rock -> acc + score_for_row j)

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 136 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
