open Base

let sum l = List.fold ~f:( + ) ~init:0 l
let product l = List.fold ~f:( * ) ~init:1 l
let fold1 ~f = function [] -> assert false | h :: t -> List.fold ~init:h ~f t

let rec unfold x ~f =
  match f x with None -> [] | Some (a, b) -> a :: unfold b ~f

module Angstrom_helpers = struct
  let number =
    let open Angstrom in
    (let+ s = take_while1 Char.is_digit in
     Int.of_string s)
    <?> "number"
end
