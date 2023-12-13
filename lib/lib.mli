val sum : int list -> int
val product : int list -> int
val fold1 : f:('a -> 'a -> 'a) -> 'a list -> 'a
val unfold : 'a -> f:('a -> ('r * 'a) option) -> 'r list

module Angstrom_helpers : sig
  val number : int Angstrom.t
end

type _ kind = All : string kind | Lines : string list kind

val main : 'i kind -> ('i -> 'a) -> ('a -> int) -> ('a -> int) -> unit
