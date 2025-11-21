type 'a t = Nil | Cons of 'a * 'a t | Err of string * Error.t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val included : ('a -> 'b -> bool) -> ('a -> bool) -> 'a t -> 'b t -> bool
val to_list : 'a t -> 'a list option
val has_err : 'a t -> bool

module type ListArg = sig
  val name : string

  type t

  val pp : Format.formatter -> t -> unit
  val registration : t Error.registration
end

module type ListSig = functor (X : ListArg) -> sig
  type nonrec t = X.t t
  type Error.t_ += List of t

  val name : string
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val of_token : Error.t -> t
  val build_token : t Error.located -> Error.t
  val iter : (X.t -> unit) -> (Error.t_ Error.located -> unit) -> t -> unit
  val mkCons : X.t -> t -> t
end

module Of : ListSig
