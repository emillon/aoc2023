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
type line = { pos : pos3; speed : speed3 } [@@deriving compare, sexp]
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
    { pos; speed }
  in
  let input = many1 line in
  parse input

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((pos (19 13 30)) (speed (-2 1 -2))) ((pos (18 19 22)) (speed (-1 -1 -2)))
     ((pos (20 25 34)) (speed (-2 -2 -4))) ((pos (12 31 28)) (speed (-1 -2 -1)))
     ((pos (20 19 15)) (speed (1 -5 -3)))) |}]

let pairs l ~compare =
  let%bind.List a = l in
  let%bind.List b = l in
  if compare a b > 0 then List.return (a, b) else []

module Mat = Owl.Dense.Matrix.D

let intersect a b =
  let open Mat in
  let mat (x, y, _) = of_array [| Float.of_int x; Float.of_int y |] 2 1 in
  let msa = mat a.speed in
  let msb = mat b.speed in
  let mpa = mat a.pos in
  let mpb = mat b.pos in
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

let w2 (a0, a1, a2) (b0, b1, b2) =
  ((a1 * b2) - (a2 * b1), (a2 * b0) - (a0 * b2), (a0 * b1) - (a1 * b0))

let mat_of_pos3 (x, y, z) =
  Mat.of_array [| Float.of_int x; Float.of_int y; Float.of_int z |] 3 1

let%expect_test "w2" =
  let t a b = w2 a b |> [%sexp_of: pos3] |> print_s in
  t (1, 3, 5) (2, 4, 6);
  [%expect {| (-2 4 -2) |}];
  t (1, 0, 0) (2, 4, 6);
  [%expect {| (0 -6 4) |}];
  t (0, 1, 0) (2, 4, 6);
  [%expect {| (6 0 -2) |}];
  t (0, 0, 1) (2, 4, 6);
  [%expect {| (-4 2 0) |}]

let vx = (1, 0, 0)
let vy = (0, 1, 0)
let vz = (0, 0, 1)
let diff_pos3 (ax, ay, az) (bx, by, bz) = (ax - bx, ay - by, az - bz)

module R2 = struct
  let a_line i j =
    let open Mat in
    let col_p v = mat_of_pos3 (w2 v (diff_pos3 i.pos j.pos)) in
    let col_s v = mat_of_pos3 (w2 v (diff_pos3 j.speed i.speed)) in
    col_s vx @|| col_s vy @|| col_s vz @|| col_p vx @|| col_p vy @|| col_p vz

  let a t =
    let open Mat in
    let t0 = List.nth_exn t 0 in
    let t1 = List.nth_exn t 1 in
    let t2 = List.nth_exn t 2 in
    a_line t0 t1 @= a_line t1 t2 @= a_line t0 t2

  let%expect_test "a" =
    parse sample |> a |> Mat.print;
    [%expect
      {|
         C0 C1 C2  C3  C4 C5
      R0  0  0  2   0   8  6
      R1  0  0  1  -8   0  1
      R2 -2 -1  0  -6  -1  0
      R3  0 -2  1   0 -12  6
      R4  2  0 -1  12   0 -2
      R5 -1  1  0  -6   2  0
      R6  0 -2  3   0  -4 12
      R7  2  0  0   4   0 -1
      R8 -3  0  0 -12   1  0 |}]

  let b_line i j = mat_of_pos3 (diff_pos3 (w2 j.pos j.speed) (w2 i.pos i.speed))

  let b t =
    let open Mat in
    let t0 = List.nth_exn t 0 in
    let t1 = List.nth_exn t 1 in
    let t2 = List.nth_exn t 2 in
    b_line t0 t1 @= b_line t1 t2 @= b_line t0 t2

  let%expect_test "b" =
    parse sample |> b |> Mat.print;
    [%expect
      {|
          C0
      R0  40
      R1  36
      R2 -44
      R3 -16
      R4  -2
      R5   9
      R6  24
      R7  34
      R8 -35 |}]

  let pos_max (x, y, z) = Int.max (Int.max x y) z

  let run t =
    let t =
      List.sort t ~compare:(fun a b ->
          Int.compare (pos_max a.pos) (pos_max b.pos))
    in
    let a = a t in
    let b = b t in
    let rank = Owl.Linalg.D.rank a in
    assert (rank = 6);
    let r = Owl.Linalg.D.linsolve a b in
    Mat.(sum' r.${[ 0; 2 ]}) |> Float.round_nearest |> Float.to_int
end

let result2 = R2.run

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 47 |}]

let run () = main All parse result result2
