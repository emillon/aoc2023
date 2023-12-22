open Base
open! Lib
open Stdio

let sample =
  [
    "1,0,1~1,2,1";
    "0,0,2~2,0,2";
    "0,2,3~2,2,3";
    "0,0,4~0,2,4";
    "2,0,5~2,2,5";
    "0,1,6~2,1,6";
    "1,1,8~1,1,9";
  ]
  |> String.concat_lines

module Pos3 = struct
  module T = struct
    type t = int * int * int [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)

  let move_down (x, y, z) = (x, y, z - 1)
end

type brick = Set.M(Pos3).t [@@deriving equal, sexp]
type t = brick list [@@deriving equal, sexp]

let set_of_brick (xa, ya, za) (xb, yb, zb) =
  let r = ref (Set.empty (module Pos3)) in
  for x = xa to xb do
    for y = ya to yb do
      for z = za to zb do
        r := Set.add !r (x, y, z)
      done
    done
  done;
  assert (not (Set.is_empty !r));
  !r

let parse =
  let open Angstrom in
  let pos3 =
    let+ x = number <* char ',' and+ y = number <* char ',' and+ z = number in
    (x, y, z)
  in
  let brick =
    let+ a = pos3 <* char '~' and+ b = pos3 <* end_of_line in
    set_of_brick a b
  in
  let input = many1 brick in
  parse input

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((1 0 1) (1 1 1) (1 2 1)) ((0 0 2) (1 0 2) (2 0 2))
     ((0 2 3) (1 2 3) (2 2 3)) ((0 0 4) (0 1 4) (0 2 4))
     ((2 0 5) (2 1 5) (2 2 5)) ((0 1 6) (1 1 6) (2 1 6)) ((1 1 8) (1 1 9))) |}]

let move_down = Set.map (module Pos3) ~f:Pos3.move_down
let intersects a b = Set.inter a b |> Set.is_empty |> not
let below b = Set.diff (move_down b) b

let%expect_test "below" =
  let test (a, b) = set_of_brick a b |> [%sexp_of: Set.M(Pos3).t] |> print_s in
  test ((0, 0, 10), (1, 0, 10));
  [%expect {| ((0 0 10) (1 0 10)) |}];
  test ((0, 0, 10), (0, 1, 10));
  [%expect {| ((0 0 10) (0 1 10)) |}];
  test ((4, 3, 5), (4, 3, 10));
  [%expect {| ((4 3 5) (4 3 6) (4 3 7) (4 3 8) (4 3 9) (4 3 10)) |}]

let is_oob s = Set.exists s ~f:(fun (_, _, z) -> z <= 0)

let settle_step t =
  let set = Set.union_list (module Pos3) t in
  List.fold_mapi t
    ~init:(Set.empty (module Int))
    ~f:(fun i acc brick ->
      let below = below brick in
      if is_oob below then (acc, brick)
      else if intersects set below then (acc, brick)
      else (Set.add acc i, move_down brick))

let settle t =
  let rec go t all_moved =
    let moved, t' = settle_step t in
    if Set.is_empty moved then (t', all_moved)
    else go t' (Set.union all_moved moved)
  in
  go t (Set.empty (module Int))

let%expect_test "settle" =
  parse sample |> settle |> [%sexp_of: t * Set.M(Int).t] |> print_s;
  [%expect
    {|
    ((((1 0 1) (1 1 1) (1 2 1)) ((0 0 2) (1 0 2) (2 0 2))
      ((0 2 2) (1 2 2) (2 2 2)) ((0 0 3) (0 1 3) (0 2 3))
      ((2 0 3) (2 1 3) (2 2 3)) ((0 1 4) (1 1 4) (2 1 4)) ((1 1 5) (1 1 6)))
     (2 3 4 5 6)) |}]

let rec removed = function
  | [] -> []
  | x :: xs -> xs :: List.map ~f:(fun t -> x :: t) (removed xs)

let%expect_test "removed" =
  removed [ 1; 2; 3; 4 ] |> [%sexp_of: int list list] |> print_s;
  [%expect {| ((2 3 4) (1 3 4) (1 2 4) (1 2 3)) |}]

let result t =
  settle t |> fst |> removed
  |> List.count ~f:(fun l ->
         let l' = settle_step l |> snd in
         [%equal: t] l l')

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 5 |}]

let result2 t =
  settle t |> fst |> removed
  |> List.map ~f:(fun l -> settle l |> snd |> Set.length)
  |> sum

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 7 |}]

let run () = main All parse result result2
