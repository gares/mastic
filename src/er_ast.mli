module Error : sig
  type 'a located

  val show_located : (Format.formatter -> 'a -> unit) -> 'a located -> string
  val pp_located : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a located -> unit
  val bloc : 'a located -> Lexing.position
  val eloc : 'a located -> Lexing.position
  val unloc : 'a located -> 'a
  val loc : 'a -> Lexing.position -> Lexing.position -> 'a located
  val view : 'a located -> 'a * Lexing.position * Lexing.position
  val map : ('a -> 'b) -> 'a located -> 'b located

  type 'a t

  val merge : 'a t -> 'a t -> 'a t
  val show : ('a -> string) -> 'a t -> string
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module ErrorToken : sig
  type t = ..

  val mkLexError : string -> Lexing.position -> Lexing.position -> t Error.located

  type 'a registration = {
    show : 'a -> string;
    match_token : t -> 'a option;
    build_token : 'a -> t;
    match_ast_error : 'a -> 'a Error.t option;
    build_ast_error : 'a Error.t -> 'a;
  }

  type register = Token : 'a registration -> register

  val show : t -> string
  val pp : Format.formatter -> t -> unit

  type 'a registered = Registered of { of_token : t Error.located -> 'a }

  val register : 'a registration -> 'a registered
  val merge : t Error.located -> t Error.located -> t Error.located
end
