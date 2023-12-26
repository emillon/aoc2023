open Base
open! Lib
open Stdio

let sample =
  [
    "19, 13, 30 @ -2,  1, -2";
    "18, 19, 22 @ -1, -1, -2";
    "20, 25, 34 @ -2, -2, -4";
    "12, 31, 28 @ -1, -2, -1";
    "20, 19, 15 @  1, -5, -3";
  ]
  |> String.concat_lines

type pos3 = int * int * int [@@deriving compare, sexp]
type speed3 = pos3 [@@deriving compare, sexp]
type line = pos3 * speed3 [@@deriving compare, sexp]
type t = line list [@@deriving sexp]

let parse =
  let open Angstrom in
  let pos3 =
    let+ x = signed_number <* string "," <* take_while1 Char.is_whitespace
    and+ y = signed_number <* string "," <* take_while1 Char.is_whitespace
    and+ z = signed_number in
    (x, y, z)
  in
  let line =
    let+ pos = pos3 <* string " @" <* take_while1 Char.is_whitespace
    and+ speed = pos3 <* end_of_line in
    (pos, speed)
  in
  let input = many1 line in
  parse input

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((19 13 30) (-2 1 -2)) ((18 19 22) (-1 -1 -2)) ((20 25 34) (-2 -2 -4))
     ((12 31 28) (-1 -2 -1)) ((20 19 15) (1 -5 -3))) |}]

let pairs l ~compare =
  let%bind.List a = l in
  let%bind.List b = l in
  if compare a b > 0 then List.return (a, b) else []

let intersect (pa, sa) (pb, sb) =
  let pxan, pyan, _ = pa in
  let pxa = Float.of_int pxan in
  let pya = Float.of_int pyan in
  let pxbn, pybn, _ = pb in
  let pxb = Float.of_int pxbn in
  let pyb = Float.of_int pybn in
  let sxan, syan, _ = sa in
  let sxa = Float.of_int sxan in
  let sya = Float.of_int syan in
  let sxbn, sybn, _ = sb in
  let sxb = Float.of_int sxbn in
  let syb = Float.of_int sybn in
  let ta =
    (((pxa -. pxb) *. syb) +. (sxb *. (pyb -. pya)))
    /. ((sxb *. sya) -. (sxa *. syb))
  in
  let tb = (pya -. pyb +. (ta *. sya)) /. syb in
  if Float.is_positive ta && Float.is_positive tb then
    let rx = pxa +. (ta *. sxa) in
    let ry = pya +. (ta *. sya) in
    Some (rx, ry)
  else None

let in_test_area ~min ~max (x, y) =
  let open Float.O in
  let ok f = min <= f && f <= max in
  ok x && ok y

let result_gen ~min ~max t =
  pairs t ~compare:[%compare: line]
  |> List.count ~f:(fun (la, lb) ->
         match intersect la lb with
         | None -> false
         | Some p -> in_test_area ~min ~max p)

let%expect_test "result" =
  parse sample |> result_gen ~min:7. ~max:27. |> printf "%d\n";
  [%expect {| 2 |}]

let result = result_gen ~min:2e14 ~max:4e14
let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
