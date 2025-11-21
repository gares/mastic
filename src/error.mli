type 'a located

val pp_located : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a located -> unit
val show_located : (Format.formatter -> 'a -> unit) -> 'a located -> string
val loc : 'a -> Lexing.position -> Lexing.position -> 'a located
val unloc : 'a located -> 'a
val bloc : 'a located -> Lexing.position
val eloc : 'a located -> Lexing.position
val view : 'a located -> 'a * Lexing.position * Lexing.position
val map : ('a -> 'b) -> 'a located -> 'b located
val omorph : ('a -> 'b option) -> 'a located -> 'b located option

type t_ = ..
type t = t_ located list

type 'a registration = {
  pp : Format.formatter -> 'a -> unit;
  match_ast : 'a -> t option;
  build_ast : t -> 'a;
  match_error : t_ -> 'a option;
  build_error : 'a -> t_;
}

type 'a registered = Registered of { of_token : t -> 'a; build_token : 'a located -> t; is_err : 'a -> bool }

val register : string -> 'a registration -> 'a registered

type t_ += Lex of string

val mkLexError : string located -> t
val span : t -> Lexing.position * Lexing.position
val merge : t -> t -> t
val squash : 'a registration -> t -> t
val show : t -> string
val pp : Format.formatter -> t -> unit
