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

let rec add_line (m, pos) dir n =
  if n = 0 then (m, pos)
  else
    let new_pos = shift pos dir in
    let new_m = Map.add_exn m ~key:new_pos ~data:() in
    add_line (new_m, new_pos) dir (n - 1)

let build_map part t =
  let empty_map = Map.empty (module Pos) in
  List.fold t
    ~init:(empty_map, (0, 0))
    ~f:(fun acc { dir; n; p2_dir; p2_n } ->
      match part with
      | `P1 -> add_line acc dir n
      | `P2 -> add_line acc p2_dir p2_n)
  |> fst

let%expect_test "build_map" =
  let m = parse sample |> build_map `P1 in
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
  let m = parse sample |> build_map `P1 in
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

let result_gen p t =
  let m = build_map p t in
  let start = exterior_start m in
  let filled = fill_from m start in
  let bounds = Map2d.bounds m in
  bounds_size bounds - Set.length filled

let result = result_gen `P1

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {|
    62 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
