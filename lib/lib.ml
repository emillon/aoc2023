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
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
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
end
