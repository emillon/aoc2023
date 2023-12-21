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
type t = rock Map2d.Dense.t [@@deriving compare, equal, sexp]

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
  parse (Map2d.Dense.parse symbol)

let view t = Map2d.Dense.view t (function Cube -> "#" | Rock -> "O")

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((Rock) () () () () (Cube) () () () ())
     ((Rock) () (Rock) (Rock) (Cube) () () () () (Cube))
     (() () () () () (Cube) (Cube) () () ())
     ((Rock) (Rock) () (Cube) (Rock) () () () () (Rock))
     (() (Rock) () () () () () (Rock) (Cube) ())
     ((Rock) () (Cube) () () (Rock) () (Cube) () (Cube))
     (() () (Rock) () () (Cube) (Rock) () () (Rock))
     (() () () () () () () (Rock) () ())
     ((Cube) () () () () (Cube) (Cube) (Cube) () ())
     ((Cube) (Rock) (Rock) () () (Cube) () () () ())) |}];
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

let next_available_in_dir t p dir =
  let rec go p =
    let dst = Dir.shift p dir in
    match Map2d.Dense.mem t dst with
    | true -> p
    | false -> go dst
    | exception Invalid_argument _ -> p
    (*else p*)
  in
  go p

let step dir t =
  let t' = Array.copy_matrix t in
  let { Map2d.imax; jmax; _ } = Map2d.Dense.bounds t in
  for i = 0 to imax do
    for j = 0 to jmax do
      let p = (i, j) in
      match Map2d.Dense.get t p with
      | None -> ()
      | Some Cube -> ()
      | Some Rock ->
          let dst = next_available_in_dir t' p dir in
          Map2d.Dense.set t' p None;
          Map2d.Dense.set t' dst (Some Rock)
    done
  done;
  t'

let%expect_test "step" =
  let t = parse sample in
  t |> step N |> view;
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

let rec fixpoint ~equal ~f x =
  let y = f x in
  if equal x y then x else fixpoint ~equal ~f y

let move_all dir t = fixpoint ~f:(step dir) ~equal:[%equal: t] t

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
  Map2d.Dense.fold t ~init:0 ~f:(fun ~key:(_i, j) ~data acc ->
      match data with Cube -> acc | Rock -> acc + score_for_row j)

let result t =
  let bounds = Map2d.Dense.bounds t in
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
  let bounds = Map2d.Dense.bounds t in
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
