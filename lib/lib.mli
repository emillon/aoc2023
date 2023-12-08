val sum : int list -> int
val product : int list -> int
val fold1 : f:('a -> 'a -> 'a) -> 'a list -> 'a

module Angstrom_helpers : sig
  val number : int Angstrom.t
end
