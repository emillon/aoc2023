open Base
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

type map = range list [@@deriving sexp]
type t = { seeds : int list; maps : map list } [@@deriving sexp]

let parse s =
  let number =
    let open Angstrom in
    (let+ s = take_while1 Char.is_digit in
     Int.of_string s)
    <?> "number"
  in
  let empty_line =
    let open Angstrom in
    let* _ = end_of_line in
    let* _ = end_of_line in
    return ()
  in
  let seeds =
    let open Angstrom in
    let* _ = string "seeds: " in
    sep_by1 (char ' ') number
  in
  let range =
    let open Angstrom in
    let* dst_start = number in
    let* _ = char ' ' in
    let* src_start = number in
    let* _ = char ' ' in
    let* length = number in
    return { dst_start; src_start; length }
  in

  let parse_map =
    let open Angstrom in
    let* _ = take_till Char.is_whitespace in
    let* _ = string " map:" in
    let* () = end_of_line in
    sep_by1 end_of_line range
  in
  let file =
    let open Angstrom in
    let* seeds in
    let* _ = empty_line in
    let* maps = sep_by1 empty_line parse_map in
    let* () = end_of_line in
    return { seeds; maps }
  in
  Angstrom.parse_string ~consume:All file s |> Result.ok_or_failwith

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    ((seeds (79 14 55 13))
     (maps
      ((((dst_start 50) (src_start 98) (length 2))
        ((dst_start 52) (src_start 50) (length 48)))
       (((dst_start 0) (src_start 15) (length 37))
        ((dst_start 37) (src_start 52) (length 2))
        ((dst_start 39) (src_start 0) (length 15)))
       (((dst_start 49) (src_start 53) (length 8))
        ((dst_start 0) (src_start 11) (length 42))
        ((dst_start 42) (src_start 0) (length 7))
        ((dst_start 57) (src_start 7) (length 4)))
       (((dst_start 88) (src_start 18) (length 7))
        ((dst_start 18) (src_start 25) (length 70)))
       (((dst_start 45) (src_start 77) (length 23))
        ((dst_start 81) (src_start 45) (length 19))
        ((dst_start 68) (src_start 64) (length 13)))
       (((dst_start 0) (src_start 69) (length 1))
        ((dst_start 1) (src_start 0) (length 69)))
       (((dst_start 60) (src_start 56) (length 37))
        ((dst_start 56) (src_start 93) (length 4)))))) |}]

let eval_map map n =
  match
    List.find_map map ~f:(fun { dst_start; src_start; length } ->
        if src_start <= n && n < src_start + length then
          Some (n - src_start + dst_start)
        else None)
  with
  | Some x -> x
  | None -> n

let rec eval maps n =
  match maps with [] -> n | map :: maps -> n |> eval_map map |> eval maps

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

let result2 _ = 0

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_all path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_all path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
