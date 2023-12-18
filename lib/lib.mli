open Base

val sum : int list -> int
val product : int list -> int
val fold1 : f:('a -> 'a -> 'a) -> 'a list -> 'a
val unfold : 'a -> f:('a -> ('r * 'a) option) -> 'r list
val number : int Angstrom.t

type _ kind = All : string kind | Lines : string list kind

val main : 'i kind -> ('i -> 'a) -> ('a -> int) -> ('a -> int) -> unit
val parse : 'a Angstrom.t -> string -> 'a

module Pos : sig
  type t = int * int [@@deriving sexp]

  include Comparable.S with type t := t
end

module Map2d : sig
  type 'a t = 'a Map.M(Pos).t [@@deriving compare, equal, sexp]

  val parse : 'a option Angstrom.t -> 'a t Angstrom.t

  type bounds = { imin : int; imax : int; jmin : int; jmax : int }

  val bounds : 'a t -> bounds
  val in_bounds : bounds -> Pos.t -> bool
  val view : ?sets:(Set.M(Pos).t * char) list -> 'a t -> ('a -> string) -> unit
end
