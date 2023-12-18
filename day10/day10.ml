open Base
open! Lib
open Stdio

let sample1 =
  [ "....."; ".S-7."; ".|.|."; ".L-J."; "....." ] |> String.concat_lines

let sample1_extra =
  [ "-L|F7"; "7S-7|"; "L|7||"; "-L-J|"; "L|-JF" ] |> String.concat_lines

let sample2 =
  [ "..F7."; ".FJ|."; "SJ.L7"; "|F--J"; "LJ..." ] |> String.concat_lines

let sample2_extra =
  [ "7-F7-"; ".FJ|7"; "SJLL7"; "|F--J"; "LJ.LJ" ] |> String.concat_lines

module Dir = struct
  type t = N | S | E | W [@@deriving equal]

  let opposite = function N -> S | S -> N | E -> W | W -> E
  let all = [ N; E; S; W ]
end

let shift (i, j) =
  let open Dir in
  function
  | N -> (i, j - 1) | S -> (i, j + 1) | E -> (i + 1, j) | W -> (i - 1, j)

let neighbours (i, j) : (Pos.t * Dir.t) list =
  [ ((i, j + 1), S); ((i + 1, j), W); ((i, j - 1), N); ((i - 1, j), E) ]

type sym = NS | EW | NE | NW | SW | SE | Start [@@deriving sexp]
type t = sym Map2d.t [@@deriving sexp]

let connected_dirs =
  let open Dir in
  function
  | NS -> [ N; S ]
  | EW -> [ E; W ]
  | NE -> [ N; E ]
  | NW -> [ N; W ]
  | SW -> [ S; W ]
  | SE -> [ S; E ]
  | Start -> [ N; S; E; W ]

let symbol_connects sym dir = List.mem (connected_dirs sym) dir ~equal:Dir.equal

let parse =
  let open Angstrom in
  let symbol =
    choice
      [
        char '.' *> return None;
        char '|' *> return (Some NS);
        char '-' *> return (Some EW);
        char 'L' *> return (Some NE);
        char 'J' *> return (Some NW);
        char '7' *> return (Some SW);
        char 'F' *> return (Some SE);
        char 'S' *> return (Some Start);
      ]
  in
  parse (Map2d.parse symbol)

let%expect_test "parse" =
  let test s = parse s |> [%sexp_of: t] |> print_s in
  test sample1;
  [%expect
    {|
    (((1 1) Start) ((1 2) NS) ((1 3) NE) ((2 1) EW) ((2 3) EW) ((3 1) SW)
     ((3 2) NS) ((3 3) NW)) |}];
  test sample1_extra;
  [%expect
    {|
    (((0 0) EW) ((0 1) SW) ((0 2) NE) ((0 3) EW) ((0 4) NE) ((1 0) NE)
     ((1 1) Start) ((1 2) NS) ((1 3) NE) ((1 4) NS) ((2 0) NS) ((2 1) EW)
     ((2 2) SW) ((2 3) EW) ((2 4) EW) ((3 0) SE) ((3 1) SW) ((3 2) NS) ((3 3) NW)
     ((3 4) NW) ((4 0) SW) ((4 1) NS) ((4 2) NS) ((4 3) NS) ((4 4) SE)) |}];
  test sample2;
  [%expect
    {|
    (((0 2) Start) ((0 3) NS) ((0 4) NE) ((1 1) SE) ((1 2) NW) ((1 3) SE)
     ((1 4) NW) ((2 0) SE) ((2 1) NW) ((2 3) EW) ((3 0) SW) ((3 1) NS) ((3 2) NE)
     ((3 3) EW) ((4 2) SW) ((4 3) NW)) |}];
  test sample2_extra;
  [%expect
    {|
    (((0 0) SW) ((0 2) Start) ((0 3) NS) ((0 4) NE) ((1 0) EW) ((1 1) SE)
     ((1 2) NW) ((1 3) SE) ((1 4) NW) ((2 0) SE) ((2 1) NW) ((2 2) NE) ((2 3) EW)
     ((3 0) SW) ((3 1) NS) ((3 2) NE) ((3 3) EW) ((3 4) NE) ((4 0) EW) ((4 1) SW)
     ((4 2) SW) ((4 3) NW) ((4 4) NW)) |}]

let find_start t =
  Map.to_alist t
  |> List.find_map_exn ~f:(fun (pos, sym) ->
         match sym with Start -> Some pos | _ -> None)

let find_all_neighbours t pos =
  let sym = Map.find_exn t pos in
  let connected_dirs = connected_dirs sym in
  List.map Dir.all ~f:(fun dir ->
      let new_pos = shift pos dir in
      let ok =
        if List.mem connected_dirs dir ~equal:Dir.equal then
          match Map.find t new_pos with
          | None -> false
          | Some new_sym -> symbol_connects new_sym (Dir.opposite dir)
        else false
      in
      (new_pos, ok))

let find_neighbours t pos =
  find_all_neighbours t pos
  |> List.filter_map ~f:(fun (p, ok) -> Option.some_if ok p)

let%expect_test _ =
  let t = parse sample1_extra in
  let start = find_start t in
  print_s [%message (start : Pos.t)];
  let n1 = find_neighbours t start in
  print_s [%message (n1 : Pos.t list)];
  [%expect {|
    (start (1 1))
    (n1 ((2 1) (1 2))) |}]

let distances t =
  let q = Queue.create () in
  let start = find_start t in
  Queue.enqueue q (start, 0);
  let distances = ref (Map.empty (module Pos)) in
  while not (Queue.is_empty q) do
    let pos, dist = Queue.dequeue_exn q in
    match Map.add !distances ~key:pos ~data:dist with
    | `Duplicate -> ()
    | `Ok m ->
        distances := m;
        find_neighbours t pos
        |> List.map ~f:(fun pos -> (pos, dist + 1))
        |> Queue.enqueue_all q
  done;
  !distances

let%expect_test "distances" =
  parse sample1 |> distances |> [%sexp_of: int Map2d.t] |> print_s;
  [%expect
    {|
    (((1 1) 0) ((1 2) 1) ((1 3) 2) ((2 1) 1) ((2 3) 3) ((3 1) 2) ((3 2) 3)
     ((3 3) 4)) |}]

let result t =
  distances t |> Map.to_alist |> List.map ~f:snd
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let%expect_test "result" =
  let test s = parse s |> result |> printf "%d\n" in
  test sample1;
  [%expect {| 4 |}];
  test sample1_extra;
  [%expect {| 4 |}];
  test sample2;
  [%expect {| 8 |}];
  test sample2_extra;
  [%expect {| 8 |}]

let sample_p2 =
  [
    ".F----7F7F7F7F-7....";
    ".|F--7||||||||FJ....";
    ".||.FJ||||||||L7....";
    "FJL7L7LJLJ||LJ.L-7..";
    "L--J.L7...LJS7F-7L7.";
    "....F-J..F7FJ|L7L7L7";
    "....L7.F7||L7|.L7L7|";
    ".....|FJLJ|FJ|F7|.LJ";
    "....FJL-7.||.||||...";
    "....L---J.LJ.LJLJ...";
  ]
  |> String.concat_lines

let exterior_start t =
  let { Map2d.imax; jmax; _ } = Map2d.bounds t in
  (List.range 0 imax |> List.concat_map ~f:(fun i -> [ (i, 0); (i, jmax) ]))
  @ (List.range 0 jmax |> List.concat_map ~f:(fun j -> [ (0, j); (imax, j) ]))
  |> List.filter ~f:(fun pos -> not (Map.mem t pos))

let%expect_test "exterior_start" =
  let t = parse sample_p2 in
  let exterior = exterior_start t in
  print_s [%message (exterior : Pos.t list)];
  [%expect
    {|
    (exterior
     ((0 0) (0 9) (1 9) (2 9) (3 9) (9 9) (12 9) (16 0) (17 0) (17 9) (18 0)
      (18 9) (0 0) (19 0) (0 1) (19 1) (0 2) (19 2) (19 3) (19 4) (0 5) (0 6)
      (0 7) (0 8) (19 8))) |}]

type ann = Next of Pos.t | Prev | Other of Pos.t option [@@deriving sexp]

let walk_cycle t =
  let loop =
    distances t
    |> Map.fold
         ~init:(Set.empty (module Pos))
         ~f:(fun ~key ~data:_ acc -> Set.add acc key)
  in
  let start = find_start t in
  let visited = ref (Set.empty (module Pos)) in
  let blue = ref (Set.empty (module Pos)) in
  let red = ref (Set.empty (module Pos)) in
  let paint color = Option.iter ~f:(fun p -> color := Set.add !color p) in
  let rec go s =
    if Set.mem !visited s then ()
    else (
      visited := Set.add !visited s;
      let ns =
        find_all_neighbours t s
        |> List.map ~f:(fun (p, ok) ->
               if ok then if Set.mem !visited p then Prev else Next p
               else if Set.mem loop p then Other None
               else Other (Some p))
      in
      match ns with
      | [ Other _; Other _; Next _; Next n ] ->
          assert (Pos.equal s start);
          go n
      | [ Other _; Next _; Next n; Other _ ] ->
          assert (Pos.equal s start);
          go n
      | [ Other r1; Other r2; Next n; Prev ] ->
          paint red r1;
          paint red r2;
          go n
      | [ Prev; Other r; Next n; Other b ] ->
          paint red r;
          paint blue b;
          go n
      | [ Prev; Next n; Other b1; Other b2 ] ->
          paint blue b1;
          paint blue b2;
          go n
      | [ Next n; Other b1; Other b2; Prev ] ->
          paint blue b1;
          paint blue b2;
          go n
      | [ Next n; Other b; Prev; Other r ] ->
          paint blue b;
          paint red r;
          go n
      | [ Other b; Next n; Prev; Other r ] ->
          paint blue b;
          paint red r;
          go n
      | [ Other b1; Other b2; Prev; Next n ] ->
          paint blue b1;
          paint blue b2;
          go n
      | [ Next n; Prev; Other r1; Other r2 ] ->
          paint red r1;
          paint red r2;
          go n
      | [ Other r; Next n; Other b; Prev ] ->
          paint red r;
          paint blue b;
          go n
      | [ Other b; Prev; Other r; Next n ] ->
          paint red r;
          paint blue b;
          go n
      | [ Other b1; Prev; Next n; Other b2 ] ->
          paint blue b1;
          paint blue b2;
          go n
      | [ Prev; Other r1; Other r2; Next n ] ->
          paint red r1;
          paint red r2;
          go n
      | [ Prev; Other _; Other _; Prev ] -> ()
      | [ Prev; Other _; Prev; Other _ ] -> ()
      | [ Other _; Other _; Prev; Prev ] -> ()
      | [ Other _; Prev; Prev; Other _ ] -> ()
      | ns -> raise_s [%message "walk_cycle" (ns : ann list)])
  in
  go start;
  (loop, !red, !blue)

let view ?(sets = []) t =
  let { Map2d.imax; jmax; _ } = Map2d.bounds t in
  for j = 0 to jmax do
    for i = 0 to imax do
      match
        List.find_map sets ~f:(fun (set, c) ->
            if Set.mem set (i, j) then Some c else None)
      with
      | Some c -> printf "%c" c
      | None -> (
          match Map.find t (i, j) with
          | Some NS -> printf "│"
          | Some EW -> printf "─"
          | Some NE -> printf "└"
          | Some NW -> printf "┘"
          | Some SW -> printf "┐"
          | Some SE -> printf "┌"
          | Some Start -> printf "*"
          | None -> printf ".")
    done;
    printf "\n"
  done

let%expect_test "walk_cycle" =
  let t = parse sample_p2 in
  let _loop, red, blue = walk_cycle t in
  view t ~sets:[ (red, 'r'); (blue, 'b') ];
  [%expect
    {|
    r┌────┐┌┐┌┐┌┐┌─┐r...
    r│┌──┐││││││││┌┘r...
    r││r┌┘││││││││└┐rr..
    ┌┘└┐└┐└┘└┘││└┘b└─┐r.
    └──┘r└┐bbb└┘*┐┌─┐└┐r
    rrrr┌─┘bb┌┐┌┘│└┐└┐└┐
    ...r└┐b┌┐││└┐│b└┐└┐│
    ....r│┌┘└┘│┌┘│┌┐│r└┘
    ...r┌┘└─┐r││r││││rrr
    ...r└───┘r└┘r└┘└┘r.. |}]

let result2 t =
  let loop, inner_start, _ = walk_cycle t in
  let bounds = Map2d.bounds t in
  let filled = ref (Set.empty (module Pos)) in
  let q = Queue.create () in
  inner_start |> Set.to_list
  |> List.filter ~f:(Map2d.in_bounds bounds)
  |> Queue.enqueue_all q;
  while not (Queue.is_empty q) do
    let pos = Queue.dequeue_exn q in
    if Map2d.in_bounds bounds pos then
      if Set.mem !filled pos then ()
      else if Set.mem loop pos then ()
      else (
        filled := Set.add !filled pos;
        let neighbours = neighbours pos |> List.map ~f:fst in
        Queue.enqueue_all q neighbours)
    else ()
  done;
  Set.length !filled

let sample_p2_2 =
  [
    "FF7FSF7F7F7F7F7F---7";
    "L|LJ||||||||||||F--J";
    "FL-7LJLJ||||||LJL-77";
    "F--JF--7||LJLJ7F7FJ-";
    "L---JF-JLJ.||-FJLJJ7";
    "|F|F-JF---7F7-L7L|7|";
    "|FFJF7L7F-JF7|JL---7";
    "7-L-JL7||F7|L7F-7F7|";
    "L.L7LFJ|||||FJL7||LJ";
    "L7JLJL-JLJLJL--JLJ.L";
  ]
  |> String.concat_lines

let%expect_test "result2" =
  let test s = parse s |> result2 |> printf "%d\n" in
  test sample_p2;
  [%expect {| 52 |}];
  test sample_p2_2;
  [%expect {| 10 |}]

let run () = main All parse result result2
