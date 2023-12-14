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
type t = rock Map.M(Pos).t [@@deriving compare, equal, sexp]

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

type dir = N | S | E | W

let shift (i, j) = function
  | N -> (i, j - 1)
  | S -> (i, j + 1)
  | W -> (i - 1, j)
  | E -> (i + 1, j)

let is_in_bounds (imax, jmax) (i, j) =
  0 <= i && i <= imax && 0 <= j && j <= jmax

let step dir bounds t =
  Map.map_keys_exn
    (module Pos)
    t
    ~f:(fun p ->
      let dst = shift p dir in
      if is_in_bounds bounds dst then
        match Map.find t p with
        | None | Some Cube -> p
        | Some Rock -> if Map.mem t dst then p else dst
      else p)

let%expect_test "step" =
  let t = parse sample in
  t |> step N (bounds t) |> view;
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

let move_all dir t =
  let bounds = bounds t in
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
  let _, jmax = bounds in
  let score_for_row j = jmax - j + 1 in
  Map.fold t ~init:0 ~f:(fun ~key:(_i, j) ~data acc ->
      match data with Cube -> acc | Rock -> acc + score_for_row j)

let result t =
  let bounds = bounds t in
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
  let bounds = bounds t in
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
