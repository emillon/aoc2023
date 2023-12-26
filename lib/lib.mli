open Base

val sum : int list -> int
val product : int list -> int
val fold1 : f:('a -> 'a -> 'a) -> 'a list -> 'a
val unfold : 'a -> f:('a -> ('r * 'a) option) -> 'r list
val number : int Angstrom.t
val signed_number : int Angstrom.t

type _ kind = All : string kind | Lines : string list kind

val main : 'i kind -> ('i -> 'a) -> ('a -> int) -> ('a -> int) -> unit
val parse : 'a Angstrom.t -> string -> 'a

module Pos : sig
  type t = int * int [@@deriving hash, sexp]

  include Comparable.S with type t := t
end

module Dir : sig
  type t = N | S | E | W [@@deriving compare, equal, hash, sexp]

  val all : t list
  val reverse : t -> t
  val shift : Pos.t -> t -> Pos.t
  val shift_n : Pos.t -> t -> int -> Pos.t
end

module Map2d : sig
  type 'a t = 'a Map.M(Pos).t [@@deriving compare, equal, sexp]

  val parse : 'a option Angstrom.t -> 'a t Angstrom.t

  type bounds = { imin : int; imax : int; jmin : int; jmax : int }
  [@@deriving sexp]

  val bounds : 'a t -> bounds
  val in_bounds : ?from_min:bool -> bounds -> Pos.t -> bool

  val view :
    ?sets:(Set.M(Pos).t * string) list -> 'a t -> ('a -> string) -> unit

  val mem : 'a t -> Pos.t -> bool
  val find_exn : 'a t -> Pos.t -> 'a
  val fold : 'a t -> init:'b -> f:(key:Pos.t -> data:'a -> 'b -> 'b) -> 'b

  module Dense : sig
    type 'a t = 'a option array array [@@deriving compare, equal, sexp]

    val mapi_option : 'a t -> f:(Pos.t -> 'a option -> 'b option) -> 'b t
    val parse : 'a option Angstrom.t -> 'a t Angstrom.t

    val view :
      ?sets:(Set.M(Pos).t * string) list -> 'a t -> ('a -> string) -> unit

    val bounds : 'a t -> bounds
    val find_exn : 'a t -> Pos.t -> 'a
    val mem : 'a t -> Pos.t -> bool
    val fold : 'a t -> init:'b -> f:(key:Pos.t -> data:'a -> 'b -> 'b) -> 'b
    val set : 'a t -> Pos.t -> 'a option -> unit
    val get : 'a t -> Pos.t -> 'a option
  end
end

val lcm : int list -> int
val fixpoint : equal:('a -> 'a -> bool) -> f:('a -> 'a) -> 'a -> 'a
