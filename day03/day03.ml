open Base
open Lib
open Stdio

let sample =
  [
    "467..114..";
    "...*......";
    "..35..633.";
    "......#...";
    "617*......";
    ".....+.58.";
    "..592.....";
    "......755.";
    "...$.*....";
    ".664.598..";
  ]

module Pos = struct
  type t = int * int [@@deriving sexp]
end

module Number = struct
  type t = { value : int; left : Pos.t; right : Pos.t } [@@deriving sexp]
end

module Symbol = struct
  type t = { pos : Pos.t; is_gear : bool } [@@deriving sexp]
end

type t = { symbols : Symbol.t list; numbers : Number.t list } [@@deriving sexp]

let empty = { symbols = []; numbers = [] }

let union a b =
  { symbols = a.symbols @ b.symbols; numbers = a.numbers @ b.numbers }

let parse_line i =
  let open Angstrom in
  let dot = char '.' <?> "dot" in
  let symbol =
    satisfy (function '.' -> false | '0' .. '9' -> false | _ -> true)
    <?> "symbol"
  in
  let element =
    choice
      [
        (let* left = pos in
         let* value = number in
         let+ right = pos in
         `Number { Number.left = (i, left); value; right = (i, right - 1) })
        <?> "element/number";
        (let* pos in
         let+ c = symbol in
         let symbol = { Symbol.pos = (i, pos); is_gear = Char.equal c '*' } in
         `Symbol symbol)
        <?> "element/symbol";
        (let+ _ = many1 dot in
         `Dots)
        <?> "element/dots";
      ]
    <?> "element"
  in
  let line =
    let+ elements = many1 element <?> "elements" in
    List.fold elements ~init:empty ~f:(fun acc element ->
        match element with
        | `Number n -> { acc with numbers = n :: acc.numbers }
        | `Symbol s -> { acc with symbols = s :: acc.symbols }
        | `Dots -> acc)
  in
  parse line

let parse lines =
  List.foldi lines ~init:empty ~f:(fun i acc line ->
      union (parse_line i line) acc)

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    ((symbols
      (((pos (8 5)) (is_gear true)) ((pos (8 3)) (is_gear false))
       ((pos (5 5)) (is_gear false)) ((pos (4 3)) (is_gear true))
       ((pos (3 6)) (is_gear false)) ((pos (1 3)) (is_gear true))))
     (numbers
      (((value 598) (left (9 5)) (right (9 7)))
       ((value 664) (left (9 1)) (right (9 3)))
       ((value 755) (left (7 6)) (right (7 8)))
       ((value 592) (left (6 2)) (right (6 4)))
       ((value 58) (left (5 7)) (right (5 8)))
       ((value 617) (left (4 0)) (right (4 2)))
       ((value 633) (left (2 6)) (right (2 8)))
       ((value 35) (left (2 2)) (right (2 3)))
       ((value 114) (left (0 5)) (right (0 7)))
       ((value 467) (left (0 0)) (right (0 2)))))) |}]

let number_touches_symbol { Number.left = li, lj; right = _, rj; _ }
    { Symbol.pos = si, sj; _ } =
  Int.abs (si - li) <= 1 && lj - 1 <= sj && sj <= rj + 1

let%expect_test "number_touches_symbol" =
  number_touches_symbol
    { Number.value = 467; left = (0, 0); right = (0, 2) }
    { pos = (1, 3); is_gear = false }
  |> [%sexp_of: bool] |> print_s;
  [%expect {| true |}]

let result t =
  List.filter_map t.numbers ~f:(fun number ->
      Option.some_if
        (List.exists t.symbols ~f:(number_touches_symbol number))
        number.value)
  |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 4361 |}]

let result2 t =
  List.fold t.symbols ~init:0 ~f:(fun acc symbol ->
      match symbol.is_gear with
      | false -> acc
      | true -> (
          let touching_numbers =
            List.filter t.numbers ~f:(fun number ->
                number_touches_symbol number symbol)
          in
          match touching_numbers with
          | [ a; b ] -> acc + (a.value * b.value)
          | _ -> acc))

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 467835 |}]

let run () = main Lines parse result result2
