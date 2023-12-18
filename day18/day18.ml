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

let shift_n (i, j) dir n =
  match dir with
  | E -> (i + n, j)
  | W -> (i - n, j)
  | N -> (i, j - n)
  | S -> (i, j + n)

type line = { dir : dir; n : int; p2_dir : dir; p2_n : int } [@@deriving sexp]
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
  let p2_n =
    let+ s = take 5 in
    String.fold s ~init:0 ~f:(fun acc c ->
        let v =
          match c with
          | '0' .. '9' -> Char.to_int c - Char.to_int '0'
          | 'a' .. 'f' -> Char.to_int c - Char.to_int 'a' + 10
          | _ -> assert false
        in
        (16 * acc) + v)
  in
  let p2_dir =
    choice
      [
        char '0' *> return E;
        char '1' *> return S;
        char '2' *> return W;
        char '3' *> return N;
      ]
  in
  let line =
    let+ dir = dir <* char ' '
    and+ n = number <* string " (#"
    and+ p2_n
    and+ p2_dir = p2_dir <* char ')' <* end_of_line in
    { dir; n; p2_n; p2_dir }
  in
  let input = many1 line in
  parse input

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((dir E) (n 6) (p2_dir E) (p2_n 461937))
     ((dir S) (n 5) (p2_dir S) (p2_n 56407))
     ((dir W) (n 2) (p2_dir E) (p2_n 356671))
     ((dir S) (n 2) (p2_dir S) (p2_n 863240))
     ((dir E) (n 2) (p2_dir E) (p2_n 367720))
     ((dir S) (n 2) (p2_dir S) (p2_n 266681))
     ((dir W) (n 5) (p2_dir W) (p2_n 577262))
     ((dir N) (n 2) (p2_dir N) (p2_n 829975))
     ((dir W) (n 1) (p2_dir W) (p2_n 112010))
     ((dir N) (n 2) (p2_dir S) (p2_n 829975))
     ((dir E) (n 2) (p2_dir W) (p2_n 491645))
     ((dir N) (n 3) (p2_dir N) (p2_n 686074))
     ((dir W) (n 2) (p2_dir W) (p2_n 5411))
     ((dir N) (n 2) (p2_dir N) (p2_n 500254))) |}]

let add_point (acc, pos) dir n =
  let other_end = shift_n pos dir n in
  (other_end :: acc, other_end)

let points part t =
  List.fold t
    ~init:([ (0, 0) ], (0, 0))
    ~f:(fun acc { dir; n; p2_dir; p2_n } ->
      match part with
      | `P1 -> add_point acc dir n
      | `P2 -> add_point acc p2_dir p2_n)
  |> fst

let rec zip_with_tail = function
  | [] -> assert false
  | [ _ ] -> []
  | x :: (y :: _ as xs) -> (x, y) :: zip_with_tail xs

let result_gen part t =
  let points = points part t in
  let segments = zip_with_tail points in
  let double_area =
    segments
    |> List.map ~f:(fun ((i1, j1), (i2, j2)) -> (i1 * j2) - (j1 * i2))
    |> sum
  in
  let a = double_area / 2 in
  let b =
    List.map segments ~f:(fun ((i1, j1), (i2, j2)) ->
        Int.abs (i1 - i2) + Int.abs (j1 - j2))
    |> sum
  in
  Int.abs a + (b / 2) + 1

let result = result_gen `P1

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {|
    62 |}]

let result2 = result_gen `P2

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 952408144115 |}]

let run () = main All parse result result2
