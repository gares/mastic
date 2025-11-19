type 'a located

val pp_located : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a located -> unit
val show_located : (Format.formatter -> 'a -> unit) -> 'a located -> string
val loc : 'a -> Lexing.position -> Lexing.position -> 'a located
val unloc : 'a located -> 'a
val bloc : 'a located -> Lexing.position
val eloc : 'a located -> Lexing.position
val view : 'a located -> 'a * Lexing.position * Lexing.position
val map : ('a -> 'b) -> 'a located -> 'b located

type 'a t_ = Lex of string | Ast of 'a
type 'a t = 'a t_ located list

val span : 'a t -> Lexing.position * Lexing.position
val merge : 'a t -> 'a t -> 'a t
val show : ('a -> string) -> 'a t -> string
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val iter : ('a located -> unit) -> (string located -> unit) -> 'a t -> unit
