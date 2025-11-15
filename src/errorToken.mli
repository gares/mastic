type t = ..

val show : t -> string
val pp : Format.formatter -> t -> unit
val mkLexError : string -> Lexing.position -> Lexing.position -> t Error.located

type 'a registration = {
  show : 'a -> string;
  match_token : t -> 'a option;
  build_token : 'a -> t;
  match_ast_error : 'a -> 'a Error.t option;
  build_ast_error : 'a Error.t -> 'a;
}

type 'a registered = Registered of { of_token : t Error.located -> 'a }

val register : 'a registration -> 'a registered
val of_token : 'a registration -> t Error.located -> 'a
val merge : t Error.located -> t Error.located -> t Error.located
