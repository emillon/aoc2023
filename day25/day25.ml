open Base
open! Lib
open Stdio

let sample =
  [
    "jqt: rhn xhk nvd";
    "rsh: frs pzl lsr";
    "xhk: hfx";
    "cmg: qnr nvd lhk bvb";
    "rhn: xhk bvb hfx";
    "bvb: xhk hfx";
    "pzl: lsr hfx nvd";
    "qnr: nvd";
    "ntq: jqt hfx bvb xhk";
    "nvd: lhk";
    "lsr: lhk";
    "rzs: qnr cmg lsr rsh";
    "frs: qnr lhk lsr";
  ]
  |> String.concat_lines

type t = string list Map.M(String).t [@@deriving sexp]

let parse =
  let open Angstrom in
  let id = take_while1 Char.is_alpha in
  let line =
    let+ src = id <* string ": "
    and+ dst = sep_by1 (string " ") id <* end_of_line in
    (src, dst)
  in
  let input =
    let+ l = many1 line in
    Map.of_alist_exn (module String) l
  in
  parse input

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    ((bvb (xhk hfx)) (cmg (qnr nvd lhk bvb)) (frs (qnr lhk lsr))
     (jqt (rhn xhk nvd)) (lsr (lhk)) (ntq (jqt hfx bvb xhk)) (nvd (lhk))
     (pzl (lsr hfx nvd)) (qnr (nvd)) (rhn (xhk bvb hfx)) (rsh (frs pzl lsr))
     (rzs (qnr cmg lsr rsh)) (xhk (hfx))) |}]

let result _ = 0

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 0 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
