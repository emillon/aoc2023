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
end

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let shift (i, j) =
    let open Dir in
    function
    | N -> (i, j - 1) | S -> (i, j + 1) | E -> (i + 1, j) | W -> (i - 1, j)

  let neighbours (i, j) : (t * Dir.t) list =
    [ ((i - 1, j), E); ((i + 1, j), W); ((i, j - 1), N); ((i, j + 1), S) ]
end

type sym = NS | EW | NE | NW | SW | SE | Start [@@deriving sexp]
type t = sym Map.M(Pos).t [@@deriving sexp]

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

let parse s =
  let symbol =
    let open Angstrom in
    let+ pos
    and+ symbol =
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
    Option.map symbol ~f:(fun s -> (s, pos))
  in
  let line =
    let open Angstrom in
    let+ s = many1 symbol <* end_of_line in
    (List.filter_opt s, List.length s)
  in
  let map =
    let open Angstrom in
    let+ ll = many1 line in
    let width = (List.hd_exn ll |> snd) + 1 in
    let off_to_pos off = (off % width, off / width) in
    List.concat_map ~f:fst ll
    |> List.map ~f:(fun (sym, off) -> (off_to_pos off, sym))
    |> Map.of_alist_exn (module Pos)
  in
  Angstrom.parse_string ~consume:All map s |> Result.ok_or_failwith

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

let find_neighbours t pos =
  let sym = Map.find_exn t pos in
  connected_dirs sym
  |> List.filter_map ~f:(fun dir ->
         let new_pos = Pos.shift pos dir in
         match Map.find t new_pos with
         | None -> None
         | Some new_sym ->
             if symbol_connects new_sym (Dir.opposite dir) then Some new_pos
             else None)

let%expect_test _ =
  let t = parse sample1_extra in
  let start = find_start t in
  print_s [%message (start : Pos.t)];
  let n1 = find_neighbours t start in
  print_s [%message (n1 : Pos.t list)];
  [%expect {|
    (start (1 1))
    (n1 ((1 2) (2 1))) |}]

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
  parse sample1 |> distances |> [%sexp_of: int Map.M(Pos).t] |> print_s;
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

let result2 _ = 0

let%expect_test "result2" =
  parse sample1 |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_all path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_all path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
