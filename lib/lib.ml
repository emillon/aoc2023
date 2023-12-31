open Base
open Stdio

let sum l = List.fold ~f:( + ) ~init:0 l
let product l = List.fold ~f:( * ) ~init:1 l
let fold1 ~f = function [] -> assert false | h :: t -> List.fold ~init:h ~f t

let rec unfold x ~f =
  match f x with None -> [] | Some (a, b) -> a :: unfold b ~f

let number =
  let open Angstrom in
  (let+ s = take_while1 Char.is_digit in
   Int.of_string s)
  <?> "number"

let signed_number =
  let open Angstrom in
  let+ sign = option Stdlib.( ~+ ) (char '-' *> return ( ~- )) and+ number in
  sign number

type _ kind = All : string kind | Lines : string list kind

let main (type i a) (kind : i kind) (parse : i -> a) result result2 =
  let input path : i =
    match kind with
    | All -> In_channel.read_all path
    | Lines -> In_channel.read_lines path
  in
  match Sys.get_argv () with
  | [| _; path |] -> input path |> parse |> result |> printf "%d\n"
  | [| _; "--2"; path |] -> input path |> parse |> result2 |> printf "%d\n"
  | _ -> assert false

let parse p s = Angstrom.parse_string ~consume:All p s |> Result.ok_or_failwith

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Dir = struct
  type t = N | S | E | W [@@deriving compare, equal, hash, sexp]

  let all = [ N; E; S; W ]
  let reverse = function N -> S | S -> N | E -> W | W -> E

  let shift (i, j) = function
    | E -> (i + 1, j)
    | W -> (i - 1, j)
    | N -> (i, j - 1)
    | S -> (i, j + 1)

  let shift_n (i, j) dir n =
    match dir with
    | E -> (i + n, j)
    | W -> (i - n, j)
    | N -> (i, j - n)
    | S -> (i, j + n)
end

module Map2d = struct
  type 'a t = 'a Map.M(Pos).t [@@deriving compare, equal, sexp]

  let parse symbol =
    let open Angstrom in
    let symbol =
      let+ pos and+ symbol in
      Option.map symbol ~f:(fun s -> (s, pos))
    in
    let line =
      let+ s = many1 symbol <* end_of_line in
      (List.filter_opt s, List.length s)
    in
    let+ pos_start = pos and+ ll = many1 line in
    let width = (List.hd_exn ll |> snd) + 1 in
    let off_to_pos off =
      let off = off - pos_start in
      (off % width, off / width)
    in
    List.concat_map ~f:fst ll
    |> List.map ~f:(fun (sym, off) -> (off_to_pos off, sym))
    |> Map.of_alist_exn (module Pos)

  type bounds = { imin : int; imax : int; jmin : int; jmax : int }
  [@@deriving sexp]

  let bounds =
    Map.fold
      ~init:
        {
          imin = Int.max_value;
          jmin = Int.max_value;
          imax = Int.min_value;
          jmax = Int.min_value;
        } ~f:(fun ~key:(i, j) ~data:_ { imin; jmin; imax; jmax } ->
        {
          imin = Int.min i imin;
          jmin = Int.min j jmin;
          imax = Int.max i imax;
          jmax = Int.max j jmax;
        })

  let in_bounds ?(from_min = false) { imax; jmax; imin; jmin } (i, j) =
    let istart, jstart = if from_min then (imin, jmin) else (0, 0) in
    i >= istart && i <= imax && j >= jstart && j <= jmax

  let view ?(sets = []) t to_string =
    let { imax; jmax; _ } = bounds t in
    for j = 0 to jmax do
      for i = 0 to imax do
        match
          List.find_map sets ~f:(fun (set, s) ->
              if Set.mem set (i, j) then Some s else None)
        with
        | Some s -> printf "%s" s
        | None -> (
            match Map.find t (i, j) with
            | Some x -> printf "%s" (to_string x)
            | None -> printf ".")
      done;
      printf "\n"
    done

  let fold = Map.fold
  let find_exn = Map.find_exn
  let mem = Map.mem

  module Dense = struct
    type 'a t = 'a option array array [@@deriving compare, equal, sexp]

    let set a (i, j) vo = a.(j).(i) <- vo
    let get m (i, j) = m.(j).(i)

    let parse f =
      let open Angstrom in
      let line =
        let+ row = many1 f <* end_of_line in
        Array.of_list row
      in
      let+ rows = many1 line in
      Array.of_list rows

    let view ?(sets = []) m f =
      Array.iteri m ~f:(fun j row ->
          Array.iteri row ~f:(fun i vo ->
              match
                List.find_map sets ~f:(fun (set, s) ->
                    if Set.mem set (i, j) then Some s else None)
              with
              | Some s -> printf "%s" s
              | None -> (
                  match vo with
                  | None -> printf "."
                  | Some v -> printf "%s" (f v)));
          printf "\n")

    let bounds m =
      let jmax = Array.length m - 1 in
      let imax = Array.length m.(0) - 1 in
      { imin = 0; jmin = 0; imax; jmax }

    let find_exn m p = get m p |> Option.value_exn
    let mem m p = get m p |> Option.is_some

    let fold a ~init ~f =
      Array.foldi a ~init ~f:(fun j acc ->
          Array.foldi ~init:acc ~f:(fun i acc ->
              Option.fold ~init:acc ~f:(fun acc data -> f ~key:(i, j) ~data acc)))

    let fold_option a ~init ~f =
      Array.foldi a ~init ~f:(fun j acc ->
          Array.foldi ~init:acc ~f:(fun i acc data -> f ~key:(i, j) ~data acc))

    let mapi_option t ~f =
      Array.mapi t ~f:(fun j row -> Array.mapi row ~f:(fun i vo -> f (i, j) vo))
  end
end

(** Return g, s, t and gcd(a,b) such that as+bt = g = gcd(a,b) *)
let rec egcd a b =
  if b = 0 then (a, 1, 0)
  else
    let d, s, t = egcd b (a % b) in
    (d, t, s - (a / b * t))

let%expect_test "egcd" =
  let test a b = egcd a b |> [%sexp_of: int * int * int] |> print_s in
  test 5 3;
  [%expect {| (1 -1 2) |}]

let lcm2 a b =
  let d, _, _ = egcd a b in
  a * b / d

let lcm l = fold1 l ~f:lcm2

let rec fixpoint ~equal ~f x =
  let y = f x in
  if equal x y then x else fixpoint ~equal ~f y
