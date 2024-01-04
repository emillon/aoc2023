open Base
open! Lib
open Stdio

module E = struct
  type t = unit [@@deriving compare]

  let default = ()
end

module G = Graph.Persistent.Graph.ConcreteLabeled (String) (E)

let build_graph t =
  Map.fold t ~init:G.empty ~f:(fun ~key:src ~data acc ->
      List.fold data ~init:acc ~f:(fun acc dst ->
          let acc = G.add_edge acc src dst in
          let acc = G.add_edge acc dst src in
          acc))

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

module Edge = struct
  module T = struct
    type t = string * string [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

let bfs g start_pos end_pos =
  let module W = struct
    type edge = G.E.t
    type t = int [@@deriving compare]

    let weight _ = 1
    let add = Int.( + )
    let zero = Int.zero
  end in
  let module D = Graph.Path.Dijkstra (G) (W) in
  fst (D.shortest_path g start_pos end_pos)
  |> List.map ~f:(fun (src, _, dst) ->
         if [%compare: string] src dst < 0 then (src, dst) else (dst, src))

let random_bfs g h keys =
  let a = List.random_element_exn keys in
  let b = List.random_element_exn keys in
  let p = bfs g a b in
  List.iter p ~f:(fun s -> Hashtbl.incr h s)

let result t =
  let h = Hashtbl.create (module Edge) in
  let take n l = List.take l n in
  let keys = Map.keys t in
  let g = build_graph t in
  for _ = 0 to 1000 do
    random_bfs g h keys
  done;
  let edges =
    Hashtbl.to_alist h
    |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a)
    |> take 3 |> List.map ~f:fst
    |> List.sort ~compare:Edge.compare
  in
  let module C = Graph.Components.Make (G) in
  List.fold edges ~init:g ~f:(fun acc (src, dst) ->
      let acc = G.remove_edge acc src dst in
      let acc = G.remove_edge acc dst src in
      acc)
  |> C.scc_list |> List.map ~f:List.length |> product

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 54 |}]

let result2 _ = 0
let run () = main All parse result result2
