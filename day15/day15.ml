open Base
open! Lib
open Stdio

let sample =
  [ "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" ]
  |> String.concat_lines

type op = Remove | Set of int [@@deriving sexp]
type step = { full : string; label : string; op : op } [@@deriving sexp]
type t = step list [@@deriving sexp]

let parse_step s =
  let open Angstrom in
  let op =
    choice
      [
        (let+ n = char '=' *> number in
         Set n);
        char '-' *> return Remove;
      ]
  in
  let step =
    let+ label = take_while1 Char.is_alpha and+ op in
    (label, op)
  in
  let label, op = parse step s in
  { label; op; full = s }

let parse s =
  s |> String.rstrip |> String.split ~on:',' |> List.map ~f:parse_step

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((full rn=1) (label rn) (op (Set 1))) ((full cm-) (label cm) (op Remove))
     ((full qp=3) (label qp) (op (Set 3))) ((full cm=2) (label cm) (op (Set 2)))
     ((full qp-) (label qp) (op Remove)) ((full pc=4) (label pc) (op (Set 4)))
     ((full ot=9) (label ot) (op (Set 9))) ((full ab=5) (label ab) (op (Set 5)))
     ((full pc-) (label pc) (op Remove)) ((full pc=6) (label pc) (op (Set 6)))
     ((full ot=7) (label ot) (op (Set 7)))) |}]

let hash_string s =
  String.fold s ~init:0 ~f:(fun acc c -> 17 * (acc + Char.to_int c) % 256)

let%expect_test "hash_string" =
  hash_string "HASH" |> [%sexp_of: int] |> print_s;
  [%expect {| 52 |}]

let hash { full; _ } = hash_string full
let result l = l |> List.map ~f:hash |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 1320 |}]

type lens = { label : string; focal : int } [@@deriving sexp]
type state = lens list Map.M(Int).t [@@deriving sexp]

let init_state = Map.empty (module Int)

let eval_step state { label; op; _ } =
  let key = hash_string label in
  match op with
  | Set focal ->
      let new_lens = { label; focal } in
      let l = Map.find state key |> Option.value ~default:[] in
      let is_target_lens lens = String.equal lens.label label in
      let data =
        if List.exists l ~f:is_target_lens then
          List.map l ~f:(fun lens ->
              if is_target_lens lens then new_lens else lens)
        else l @ [ { label; focal } ]
      in
      Map.set state ~key ~data
  | Remove ->
      Map.update state key ~f:(fun lo ->
          match lo with
          | None -> []
          | Some l ->
              List.filter l ~f:(fun lens -> not (String.equal lens.label label)))

let eval steps = List.fold steps ~init:init_state ~f:eval_step

let%expect_test "eval" =
  parse sample |> eval |> [%sexp_of: state] |> print_s;
  [%expect
    {|
    ((0 (((label rn) (focal 1)) ((label cm) (focal 2)))) (1 ())
     (3 (((label ot) (focal 7)) ((label ab) (focal 5)) ((label pc) (focal 6))))) |}]

let score : state -> int =
  Map.fold ~init:0 ~f:(fun ~key ~data acc ->
      List.foldi data ~init:acc ~f:(fun i acc { focal; _ } ->
          acc + ((key + 1) * (i + 1) * focal)))

let result2 l = eval l |> score

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 145 |}]

let run () = main All parse result result2
