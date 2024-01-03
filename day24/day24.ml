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

module Mat = Owl.Dense.Matrix.D

let intersect (pa, sa) (pb, sb) =
  let open Mat in
  let mat (x, y, _) = of_array [| Float.of_int x; Float.of_int y |] 2 1 in
  let msa = mat sa in
  let msb = mat sb in
  let mpa = mat pa in
  let mpb = mat pb in
  match Owl.Linalg.D.linsolve (msa @|| neg msb) (mpb - mpa) with
  | exception Failure _ -> None
  | t ->
      Option.some_if
        (for_all Float.is_positive t)
        (mpa + ((msa @|| zeros 2 1) *@ t))

let in_test_area ~min ~max p =
  let open Float.O in
  let ok f = min <= f && f <= max in
  Mat.for_all ok p

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
