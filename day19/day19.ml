open Base
open! Lib
open Stdio

let sample =
  [
    "px{a<2006:qkq,m>2090:A,rfg}";
    "pv{a>1716:R,A}";
    "lnx{m>1548:A,A}";
    "rfg{s<537:gd,x>2440:R,A}";
    "qs{s>3448:A,lnx}";
    "qkq{x<1416:A,crn}";
    "crn{x>2662:A,R}";
    "in{s<1351:px,qqz}";
    "qqz{s>2770:qs,m<1801:hdj,R}";
    "gd{a>3333:R,R}";
    "hdj{m>838:A,pv}";
    "";
    "{x=787,m=2655,a=1222,s=2876}";
    "{x=1679,m=44,a=2067,s=496}";
    "{x=2036,m=264,a=79,s=2244}";
    "{x=2461,m=1339,a=466,s=291}";
    "{x=2127,m=1623,a=2188,s=1013}";
  ]
  |> String.concat_lines

type variable = X | M | A | S [@@deriving sexp]
type op = Lt | Gt [@@deriving sexp]

type condition = Op of { op : op; variable : variable; rhs : int } | Always
[@@deriving sexp]

type outcome = Goto of string | Return of bool [@@deriving sexp]
type statement = { condition : condition; outcome : outcome } [@@deriving sexp]
type input = { x : int; m : int; a : int; s : int } [@@deriving sexp]

type t = { workflows : statement list Map.M(String).t; inputs : input list }
[@@deriving sexp]

let parse =
  let open Angstrom in
  let name = take_while1 Char.is_lowercase in
  let variable =
    choice
      [
        char 'x' *> return X;
        char 'm' *> return M;
        char 'a' *> return A;
        char 's' *> return S;
      ]
  in
  let op = choice [ char '<' *> return Lt; char '>' *> return Gt ] in
  let condition =
    let+ variable and+ op and+ rhs = number <* char ':' in
    Op { op; variable; rhs }
  in
  let outcome =
    choice
      [
        (let+ name in
         Goto name);
        char 'R' *> return (Return false);
        char 'A' *> return (Return true);
      ]
  in
  let statement =
    let+ condition = option Always condition and+ outcome in
    { condition; outcome }
  in
  let workflow =
    let+ name
    and+ statements =
      char '{' *> sep_by1 (char ',') statement <* char '}' <* end_of_line
    in
    (name, statements)
  in
  let input =
    let+ x = string "{x=" *> number
    and+ m = string ",m=" *> number
    and+ a = string ",a=" *> number
    and+ s = string ",s=" *> number <* char '}' *> end_of_line in
    { x; m; a; s }
  in
  let whole_input =
    let+ workflows = many1 workflow <* end_of_line and+ inputs = many1 input in
    let workflows = Map.of_alist_exn (module String) workflows in
    { workflows; inputs }
  in
  parse whole_input

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    ((workflows
      ((crn
        (((condition (Op (op Gt) (variable X) (rhs 2662)))
          (outcome (Return true)))
         ((condition Always) (outcome (Return false)))))
       (gd
        (((condition (Op (op Gt) (variable A) (rhs 3333)))
          (outcome (Return false)))
         ((condition Always) (outcome (Return false)))))
       (hdj
        (((condition (Op (op Gt) (variable M) (rhs 838)))
          (outcome (Return true)))
         ((condition Always) (outcome (Goto pv)))))
       (in
        (((condition (Op (op Lt) (variable S) (rhs 1351))) (outcome (Goto px)))
         ((condition Always) (outcome (Goto qqz)))))
       (lnx
        (((condition (Op (op Gt) (variable M) (rhs 1548)))
          (outcome (Return true)))
         ((condition Always) (outcome (Return true)))))
       (pv
        (((condition (Op (op Gt) (variable A) (rhs 1716)))
          (outcome (Return false)))
         ((condition Always) (outcome (Return true)))))
       (px
        (((condition (Op (op Lt) (variable A) (rhs 2006))) (outcome (Goto qkq)))
         ((condition (Op (op Gt) (variable M) (rhs 2090)))
          (outcome (Return true)))
         ((condition Always) (outcome (Goto rfg)))))
       (qkq
        (((condition (Op (op Lt) (variable X) (rhs 1416)))
          (outcome (Return true)))
         ((condition Always) (outcome (Goto crn)))))
       (qqz
        (((condition (Op (op Gt) (variable S) (rhs 2770))) (outcome (Goto qs)))
         ((condition (Op (op Lt) (variable M) (rhs 1801))) (outcome (Goto hdj)))
         ((condition Always) (outcome (Return false)))))
       (qs
        (((condition (Op (op Gt) (variable S) (rhs 3448)))
          (outcome (Return true)))
         ((condition Always) (outcome (Goto lnx)))))
       (rfg
        (((condition (Op (op Lt) (variable S) (rhs 537))) (outcome (Goto gd)))
         ((condition (Op (op Gt) (variable X) (rhs 2440)))
          (outcome (Return false)))
         ((condition Always) (outcome (Return true)))))))
     (inputs
      (((x 787) (m 2655) (a 1222) (s 2876)) ((x 1679) (m 44) (a 2067) (s 496))
       ((x 2036) (m 264) (a 79) (s 2244)) ((x 2461) (m 1339) (a 466) (s 291))
       ((x 2127) (m 1623) (a 2188) (s 1013))))) |}]

let eval_variable input = function
  | X -> input.x
  | M -> input.m
  | A -> input.a
  | S -> input.s

let eval_condition input = function
  | Op { variable; op; rhs } -> (
      let lhs = eval_variable input variable in
      match op with Lt -> lhs < rhs | Gt -> lhs > rhs)
  | Always -> true

let rec eval_outcome l input =
  match l with
  | [] -> assert false
  | { condition; outcome } :: otherwise ->
      if eval_condition input condition then outcome
      else eval_outcome otherwise input

let rec eval m input name =
  let s = Map.find_exn m name in
  match eval_outcome s input with
  | Goto new_name -> eval m input new_name
  | Return b -> b

let value { x; m; a; s } = x + m + a + s

let result { workflows; inputs } =
  List.filter_map inputs ~f:(fun input ->
      if eval workflows input "in" then Some (value input) else None)
  |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 19114 |}]

type diet = Diet.Int.t

let sexp_of_diet d =
  Diet.Int.fold
    (fun i acc -> (Diet.Int.Interval.x i, Diet.Int.Interval.y i) :: acc)
    d []
  |> List.rev |> [%sexp_of: (int * int) list]

let equal_diet = Diet.Int.equal

type abstract = { x : diet; m : diet; a : diet; s : diet }
[@@deriving equal, sexp_of]

let cardinal { x; m; a; s } =
  Diet.Int.cardinal x * Diet.Int.cardinal m * Diet.Int.cardinal a
  * Diet.Int.cardinal s

let bottom =
  let empty = Diet.Int.empty in
  { x = empty; m = empty; a = empty; s = empty }

let from_interval x y =
  let i = Diet.Int.Interval.make x y in
  Diet.Int.add i Diet.Int.empty

let top =
  let min = 1 in
  let max = 4000 in
  let d = from_interval min max in
  { x = d; m = d; a = d; s = d }

let lift_var t ~f = function
  | X -> { t with x = f t.x }
  | M -> { t with m = f t.m }
  | A -> { t with a = f t.a }
  | S -> { t with s = f t.s }

let set_min v n t =
  lift_var t v ~f:(Diet.Int.inter (from_interval n Int.max_value))

let set_max v n t = lift_var t v ~f:(Diet.Int.inter (from_interval 0 n))

let is_true c cs =
  match c with
  | Always -> cs
  | Op { op = Gt; variable; rhs } -> set_min variable (rhs + 1) cs
  | Op { op = Lt; variable; rhs } -> set_max variable (rhs - 1) cs

let is_false c cs =
  match c with
  | Always -> bottom
  | Op { op = Lt; variable; rhs } -> set_min variable rhs cs
  | Op { op = Gt; variable; rhs } -> set_max variable rhs cs

let eval_abstract cs m name =
  let named = Map.find_exn m in
  let rec go cs l =
    match l with
    | [] -> 0
    | { condition; outcome = Return false } :: otherwise ->
        go (is_false condition cs) otherwise
    | { condition; outcome = Return true } :: otherwise ->
        cardinal (is_true condition cs) + go (is_false condition cs) otherwise
    | { condition; outcome = Goto name } :: otherwise ->
        go (is_true condition cs) (named name)
        + go (is_false condition cs) otherwise
  in
  go cs (named name)

let result2 t = eval_abstract top t.workflows "in"

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 167409079868000 |}]

let run () = main All parse result result2
