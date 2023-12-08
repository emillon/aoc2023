open Base
open! Lib
open Stdio

let sample = [ "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" ]

type card = Joker | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | T | J | Q | K | A
[@@deriving compare, equal, sexp]

type hand = card list [@@deriving sexp]
type line = { hand : hand; bid : int } [@@deriving sexp]
type t = line list [@@deriving sexp]

let parse_card = function
  | '2' -> Some C2
  | '3' -> Some C3
  | '4' -> Some C4
  | '5' -> Some C5
  | '6' -> Some C6
  | '7' -> Some C7
  | '8' -> Some C8
  | '9' -> Some C9
  | 'T' -> Some T
  | 'J' -> Some J
  | 'Q' -> Some Q
  | 'K' -> Some K
  | 'A' -> Some A
  | ' ' -> None
  | c -> raise_s [%message "parse_card" (c : char)]

let hand_of_string s =
  String.to_list s |> List.map ~f:(fun c -> parse_card c |> Option.value_exn)

let parse_line s =
  let open Angstrom_helpers in
  let hand =
    let open Angstrom in
    let+ s = take_while1 (fun c -> Option.is_some (parse_card c)) in
    hand_of_string s
  in
  let line =
    let open Angstrom in
    let+ hand and+ bid = char ' ' *> number in
    { hand; bid }
  in
  Angstrom.parse_string ~consume:All line s |> Result.ok_or_failwith

let parse l = List.map l ~f:parse_line

let%expect_test "parse" =
  parse sample |> [%sexp_of: t] |> print_s;
  [%expect
    {|
    (((hand (C3 C2 T C3 K)) (bid 765)) ((hand (T C5 C5 J C5)) (bid 684))
     ((hand (K K C6 C7 C7)) (bid 28)) ((hand (K T J J T)) (bid 220))
     ((hand (Q Q Q J A)) (bid 483))) |}]

type hand_type =
  | High_card
  | One_pair
  | Two_pairs
  | Three_of_a_kind
  | Full_house
  | Four_of_a_kind
  | Five_of_a_kind
[@@deriving compare, sexp]

let add_last delta = function
  | [] -> [ delta ]
  | l ->
      let rec go = function
        | [] -> []
        | [ n ] -> [ n + delta ]
        | x :: xs -> x :: go xs
      in
      go l

let card_counts l =
  let jokers, non_jokers = List.partition_tf l ~f:(equal_card Joker) in
  let njokers = List.length jokers in
  ( non_jokers
    |> List.sort ~compare:compare_card
    |> List.group ~break:(fun a b -> not (equal_card a b))
    |> List.map ~f:List.length
    (*|> add_last (njokers)*)
    |> List.sort ~compare:Int.compare,
    njokers )

let hand_type l =
  match card_counts l with
  | [ 5 ], 0 | [ 4 ], 1 | [ 3 ], 2 | [ 2 ], 3 | [ 1 ], 4 | [], 5 ->
      Five_of_a_kind
  | [ 1; 4 ], 0 | [ 1; 3 ], 1 | [ 1; 2 ], 2 | [ 1; 1 ], 3 -> Four_of_a_kind
  | [ 2; 3 ], 0 | [ 2; 2 ], 1 -> Full_house
  | [ 1; 1; 3 ], 0 | [ 1; 1; 1 ], 2 | [ 1; 1; 2 ], 1 -> Three_of_a_kind
  | [ 1; 2; 2 ], 0 -> Two_pairs
  | [ 1; 1; 1; 2 ], 0 | [ 1; 1; 1; 1 ], 1 -> One_pair
  | [ 1; 1; 1; 1; 1 ], 0 -> High_card
  | cs, jokers -> raise_s [%message "hand_type" (cs : int list) (jokers : int)]

let%expect_test "hand_type" =
  let test s =
    hand_of_string s |> hand_type |> [%sexp_of: hand_type] |> print_s
  in
  test "AAAAA";
  [%expect {| Five_of_a_kind |}];
  test "QQQQQ";
  [%expect {| Five_of_a_kind |}];
  test "AA8AA";
  [%expect {| Four_of_a_kind |}];
  test "23332";
  [%expect {| Full_house |}];
  test "TTT98";
  [%expect {| Three_of_a_kind |}];
  test "23432";
  [%expect {| Two_pairs |}];
  test "A23A4";
  [%expect {| One_pair |}];
  test "23456";
  [%expect {| High_card |}]

let compare_hand a b =
  let ta = hand_type a in
  let tb = hand_type b in
  match compare_hand_type ta tb with 0 -> [%compare: card list] a b | n -> n

let%expect_test "compare_hands" =
  parse sample
  |> List.map ~f:(fun t -> t.hand)
  |> List.sort ~compare:compare_hand
  |> [%sexp_of: hand list] |> print_s;
  [%expect
    {| ((C3 C2 T C3 K) (K T J J T) (K K C6 C7 C7) (T C5 C5 J C5) (Q Q Q J A)) |}]

let result l =
  List.sort l ~compare:(fun a b -> compare_hand a.hand b.hand)
  |> List.mapi ~f:(fun i { bid; _ } ->
         let rank = i + 1 in
         rank * bid)
  |> sum

let%expect_test "result" =
  parse sample |> result |> printf "%d\n";
  [%expect {| 6440 |}]

let set_jokers l =
  List.map l ~f:(fun l ->
      { l with hand = List.map l.hand ~f:(function J -> Joker | c -> c) })

let result2 l = l |> set_jokers |> result

let%expect_test "result2" =
  parse sample |> result2 |> printf "%d\n";
  [%expect {| 5905 |}]

let run () =
  match Sys.get_argv () with
  | [| _; path |] ->
      In_channel.read_lines path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] ->
      In_channel.read_lines path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false
