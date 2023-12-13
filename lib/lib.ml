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
