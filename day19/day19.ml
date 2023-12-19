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

let eval workflows input =
  let rec go name input =
    let s = Map.find_exn workflows name in
    let outcome = eval_outcome s input in
    match outcome with Goto new_name -> go new_name input | Return b -> b
  in
  go "in" input

let value { x; m; a; s } = x + m + a + s

let result { workflows; inputs } =
  List.filter_map inputs ~f:(fun input ->
      if eval workflows input then Some (value input) else None)
  |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 19114 |}]

let result2 _ = 0

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 0 |}]

let run () = main All parse result result2
