open Base
open! Lib
open Stdio

let sample1 =
  [ "broadcaster -> a, b, c"; "%a -> b"; "%b -> c"; "%c -> inv"; "&inv -> a" ]
  |> String.concat_lines

let sample2 =
  [
    "broadcaster -> a";
    "%a -> inv, con";
    "&inv -> b";
    "%b -> con";
    "&con -> output";
  ]
  |> String.concat_lines

type node_type = Broadcaster | Conjunction | Flipflop [@@deriving sexp]
type node = { name : string; node_type : node_type } [@@deriving sexp]
type line = { src : node; dsts : string list } [@@deriving sexp]
type t = line list [@@deriving sexp]

let parse =
  let open Angstrom in
  let name = take_while1 Char.is_lowercase in
  let node_type =
    choice [ char '%' *> return Flipflop; char '&' *> return Conjunction ]
  in
  let node =
    let+ node_type = option Broadcaster node_type and+ name in
    { name; node_type }
  in
  let line =
    let+ src = node <* string " -> "
    and+ dsts = sep_by1 (string ", ") name <* end_of_line in
    { src; dsts }
  in
  let input = many1 line in
  parse input

let%expect_test "parse" =
  parse sample1 |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((src ((name broadcaster) (node_type Broadcaster))) (dsts (a b c)))
     ((src ((name a) (node_type Flipflop))) (dsts (b)))
     ((src ((name b) (node_type Flipflop))) (dsts (c)))
     ((src ((name c) (node_type Flipflop))) (dsts (inv)))
     ((src ((name inv) (node_type Conjunction))) (dsts (a)))) |}]

type level = Low | High

let toggle = function Low -> High | High -> Low
let is_high = function High -> true | Low -> false
let level_to_string = function Low -> "low" | High -> "high"

module Bus = struct
  type active = (string -> level -> unit) Staged.t

  type t = {
    on_event : src:string -> dst:string -> level -> unit;
    find : string -> active;
    queue : (string * string * level) Queue.t;
  }

  let create ~on_event ~find =
    let queue = Queue.create () in
    { on_event; find; queue }

  let send_to_all t dsts src level =
    List.iter dsts ~f:(fun dst -> Queue.enqueue t.queue (src, dst, level))

  let fire t (src, dst, level) =
    let a = t.find dst in
    let af = Staged.unstage a in
    t.on_event ~src ~dst level;
    af src level

  let fire_next t =
    let e = Queue.dequeue_exn t.queue in
    fire t e

  let run t =
    while not (Queue.is_empty t.queue) do
      fire_next t
    done
end

let broadcast b name dsts =
  Staged.stage (fun _src level -> Bus.send_to_all b dsts name level)

let flipflop b name dsts =
  let state = ref Low in
  Staged.stage (fun _src level ->
      match level with
      | High -> ()
      | Low ->
          Ref.replace state toggle;
          Bus.send_to_all b dsts name !state)

let conjunction b name srcs dsts =
  let state =
    List.map srcs ~f:(fun src -> (src, Low))
    |> Map.of_alist_exn (module String)
    |> ref
  in
  Staged.stage (fun src level ->
      let new_state = Map.set !state ~key:src ~data:level in
      state := new_state;
      let out_level = if Map.for_all new_state ~f:is_high then Low else High in
      Bus.send_to_all b dsts name out_level)

let output_node = Staged.stage (fun _ _ -> ())

let find_srcs l dst =
  List.filter_map l ~f:(fun { src = { name; _ }; dsts } ->
      Option.some_if (List.mem dsts dst ~equal:String.equal) name)

let setup_bus lines ~f =
  let nodes = ref (Map.empty (module String)) in
  let find name =
    match Map.find !nodes name with Some x -> x | None -> output_node
  in
  let bus = Bus.create ~find ~on_event:f in
  let node_list =
    List.map lines ~f:(fun { src = { name; node_type }; dsts } ->
        let a =
          match node_type with
          | Broadcaster -> broadcast bus name dsts
          | Flipflop -> flipflop bus name dsts
          | Conjunction ->
              let srcs = find_srcs lines name in
              conjunction bus name srcs dsts
        in
        (name, a))
  in
  nodes := Map.of_alist_exn (module String) node_list;
  let broadcast = find "broadcaster" |> Staged.unstage in
  let press_button () = broadcast "button" Low in
  (bus, press_button)

let trace_event ~src ~dst level =
  printf "%s -[%s]-> %s\n" src (level_to_string level) dst

let trace l =
  let bus, press_button = setup_bus l ~f:trace_event in
  press_button ();
  Bus.run bus

let%expect_test "trace" =
  let test s = parse s |> trace in
  test sample1;
  [%expect
    {|
    broadcaster -[low]-> a
    broadcaster -[low]-> b
    broadcaster -[low]-> c
    a -[high]-> b
    b -[high]-> c
    c -[high]-> inv
    inv -[low]-> a
    a -[low]-> b
    b -[low]-> c
    c -[low]-> inv
    inv -[high]-> a |}];
  test sample2;
  [%expect
    {|
    broadcaster -[low]-> a
    a -[high]-> inv
    a -[high]-> con
    inv -[low]-> b
    con -[high]-> output
    b -[high]-> con
    con -[low]-> output |}]

let result l =
  let low = ref 0 in
  let high = ref 0 in
  let bus, press_button =
    setup_bus l ~f:(fun ~src:_ ~dst:_ level ->
        match level with Low -> Int.incr low | High -> Int.incr high)
  in
  for _ = 1 to 1000 do
    Int.incr low;
    press_button ();
    Bus.run bus
  done;
  !low * !high

let%expect_test "result" =
  let test s = parse s |> result |> printf "%d\n" in
  test sample1;
  [%expect {| 32000000 |}];
  test sample2;
  [%expect {| 11687500 |}]

let only = function [ x ] -> x | _ -> assert false

let cycle_len l n =
  let ok = ref false in
  let bus, press_button =
    setup_bus l ~f:(fun ~src:_ ~dst level ->
        match level with Low when String.equal dst n -> ok := true | _ -> ())
  in
  let i = ref 0 in
  while not !ok do
    Int.incr i;
    press_button ();
    Bus.run bus
  done;
  !i

let result2 l =
  find_srcs l "rx" |> only |> find_srcs l |> List.map ~f:(cycle_len l) |> lcm

let run () = main All parse result result2
