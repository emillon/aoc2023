open Base
open Lib
open Stdio

let sample =
  [
    "seeds: 79 14 55 13";
    "";
    "seed-to-soil map:";
    "50 98 2";
    "52 50 48";
    "";
    "soil-to-fertilizer map:";
    "0 15 37";
    "37 52 2";
    "39 0 15";
    "";
    "fertilizer-to-water map:";
    "49 53 8";
    "0 11 42";
    "42 0 7";
    "57 7 4";
    "";
    "water-to-light map:";
    "88 18 7";
    "18 25 70";
    "";
    "light-to-temperature map:";
    "45 77 23";
    "81 45 19";
    "68 64 13";
    "";
    "temperature-to-humidity map:";
    "0 69 1";
    "1 0 69";
    "";
    "humidity-to-location map:";
    "60 56 37";
    "56 93 4";
  ]
  |> String.concat_lines

type range = { dst_start : int; src_start : int; length : int }
[@@deriving sexp]

type range2 = { start : int; end_ : int; offset : int } [@@deriving sexp]
type map = range2 list [@@deriving sexp]
type t = { seeds : int list; maps : map list } [@@deriving sexp]

let build_full_map prev_max ranges =
  let moved =
    List.fold ranges ~init:Diet.Int.empty
      ~f:(fun acc { src_start; length; dst_start = _ } ->
        Diet.Int.add
          (Diet.Int.Interval.make src_start (src_start + length - 1))
          acc)
  in
  let max = Int.max prev_max (Diet.Int.max_elt moved |> Diet.Int.Interval.y) in
  let missing =
    Diet.Int.diff
      (Diet.Int.add (Diet.Int.Interval.make 0 max) Diet.Int.empty)
      moved
  in
  let from_missing =
    Diet.Int.fold
      (fun interval acc ->
        let x = Diet.Int.Interval.x interval in
        let y = Diet.Int.Interval.y interval in
        { start = x; end_ = y; offset = 0 } :: acc)
      missing []
  in
  let r =
    List.map ranges ~f:(fun { src_start; length; dst_start } ->
        {
          start = src_start;
          end_ = src_start + length - 1;
          offset = dst_start - src_start;
        })
    @ from_missing
    |> List.sort ~compare:(fun a b -> Int.compare a.start b.start)
  in
  (r, max)

let parse s =
  let open Angstrom_helpers in
  let empty_line =
    let open Angstrom in
    end_of_line *> end_of_line
  in
  let seeds =
    let open Angstrom in
    string "seeds: " *> sep_by1 (char ' ') number
  in
  let range =
    let open Angstrom in
    let+ dst_start = number <* char ' '
    and+ src_start = number <* char ' '
    and+ length = number in
    { dst_start; src_start; length }
  in
  let parse_map =
    let open Angstrom in
    let+ map =
      take_till Char.is_whitespace
      *> string " map:" *> end_of_line *> sep_by1 end_of_line range
    in
    map
  in
  let file =
    let open Angstrom in
    let+ seeds = seeds <* empty_line
    and+ maps = sep_by1 empty_line parse_map <* end_of_line in
    let maps, _ =
      List.fold maps ~init:([], 0) ~f:(fun (acc_l, max_size) map ->
          let m, new_max_size = build_full_map max_size map in
          (m :: acc_l, new_max_size))
    in
    { seeds; maps = List.rev maps }
  in
  Angstrom.parse_string ~consume:All file s |> Result.ok_or_failwith

let%expect_test "build_full_map" =
  build_full_map 0
    [
      { dst_start = 50; src_start = 98; length = 2 };
      { dst_start = 52; src_start = 50; length = 48 };
    ]
  |> [%sexp_of: range2 list * int] |> print_s;
  [%expect
    {|
    ((((start 0) (end_ 49) (offset 0)) ((start 50) (end_ 97) (offset 2))
      ((start 98) (end_ 99) (offset -48)))
     99) |}]

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
  ((seeds (79 14 55 13))
   (maps
    ((((start 0) (end_ 49) (offset 0)) ((start 50) (end_ 97) (offset 2))
      ((start 98) (end_ 99) (offset -48)))
     (((start 0) (end_ 14) (offset 39)) ((start 15) (end_ 51) (offset -15))
      ((start 52) (end_ 53) (offset -15)) ((start 54) (end_ 99) (offset 0)))
     (((start 0) (end_ 6) (offset 42)) ((start 7) (end_ 10) (offset 50))
      ((start 11) (end_ 52) (offset -11)) ((start 53) (end_ 60) (offset -4))
      ((start 61) (end_ 99) (offset 0)))
     (((start 0) (end_ 17) (offset 0)) ((start 18) (end_ 24) (offset 70))
      ((start 25) (end_ 94) (offset -7)) ((start 95) (end_ 99) (offset 0)))
     (((start 0) (end_ 44) (offset 0)) ((start 45) (end_ 63) (offset 36))
      ((start 64) (end_ 76) (offset 4)) ((start 77) (end_ 99) (offset -32)))
     (((start 0) (end_ 68) (offset 1)) ((start 69) (end_ 69) (offset -69))
      ((start 70) (end_ 99) (offset 0)))
     (((start 0) (end_ 55) (offset 0)) ((start 56) (end_ 92) (offset 4))
      ((start 93) (end_ 96) (offset -37)) ((start 97) (end_ 99) (offset 0)))))) |}]

let num_ranges d = Diet.Int.fold (fun _ acc -> acc + 1) d 0

let interval_add_offset i offset =
  let x = Diet.Int.Interval.x i in
  let y = Diet.Int.Interval.y i in
  Diet.Int.Interval.make (x + offset) (y + offset)

let diet_add_offset d offset =
  Diet.Int.fold
    (fun interval acc -> Diet.Int.add (interval_add_offset interval offset) acc)
    d Diet.Int.empty

let eval_range_one i r =
  let d1 = Diet.Int.add i Diet.Int.empty in
  let d2 =
    Diet.Int.add (Diet.Int.Interval.make r.start r.end_) Diet.Int.empty
  in
  let d = Diet.Int.inter d1 d2 in
  diet_add_offset d r.offset

let eval_range r map =
  List.fold map ~init:Diet.Int.empty ~f:(fun acc range ->
      Diet.Int.union (eval_range_one r range) acc)

type diet = Diet.Int.t

let sexp_of_diet d =
  Diet.Int.fold
    (fun i acc ->
      let x = Diet.Int.Interval.x i in
      let y = Diet.Int.Interval.y i in
      (x, y) :: acc)
    d []
  |> List.rev |> [%sexp_of: (int * int) list]

let%expect_test "eval_range" =
  let t = parse sample in
  let a = List.hd_exn t.maps in
  let test (start, len) =
    let x = Diet.Int.Interval.make start (start + len - 1) in
    eval_range x a |> [%sexp_of: diet] |> print_s
  in
  test (0, 50);
  [%expect {| ((0 49)) |}];
  test (0, 52);
  [%expect {| ((0 49) (52 53)) |}];
  test (2, 52);
  [%expect {| ((2 49) (52 55)) |}]

let eval_map n ranges =
  List.find_map_exn ranges ~f:(fun { start; end_; offset } ->
      Option.some_if (n >= start && n <= end_) (n + offset))

let eval maps n = List.fold ~f:eval_map ~init:n maps

let%expect_test "eval" =
  let { maps; _ } = parse sample in
  let test n = eval maps n |> printf "%d\n" in
  test 79;
  [%expect {| 82 |}];
  test 14;
  [%expect {| 43 |}];
  test 55;
  [%expect {| 86 |}];
  test 13;
  [%expect {| 35 |}]

let result t =
  t.seeds
  |> List.map ~f:(eval t.maps)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

let rec to_ranges = function
  | start :: length :: seeds ->
      Diet.Int.add
        (Diet.Int.Interval.make start (start + length - 1))
        (to_ranges seeds)
  | [] -> Diet.Int.empty
  | _ -> assert false

let eval_all_ranges maps ranges =
  List.fold maps ~init:ranges ~f:(fun acc_ranges map ->
      Diet.Int.fold
        (fun interval acc -> Diet.Int.union (eval_range interval map) acc)
        acc_ranges Diet.Int.empty)

let%expect_test "eval_all_ranges" =
  let { maps; _ } = parse sample in
  let test l = eval_all_ranges maps l |> [%sexp_of: diet] |> print_s in
  test (Diet.Int.add (Diet.Int.Interval.make 0 99) Diet.Int.empty);
  [%expect {|
    ((0 99)) |}]

let result2 t =
  t.seeds |> to_ranges |> eval_all_ranges t.maps |> Diet.Int.min_elt
  |> Diet.Int.Interval.x

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 46 |}]

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_all path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_all path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
