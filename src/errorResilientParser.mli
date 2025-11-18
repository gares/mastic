val debug : bool ref

type 'token tok = { s : string; t : 'token; b : Lexing.position; e : Lexing.position }

type ('token, 'production) recovery_action =
  | TurnIntoError
  | GenerateHole
  | GenerateToken of 'token tok
  | Reduce of 'production

module type Recovery = sig
  type token

  val show_token : token -> string

  type 'a symbol
  type xsymbol

  val show_symbol : 'a option -> 'a symbol -> string

  type 'a terminal
  type 'a env
  type production

  val token_of_terminal : 'a terminal -> (string * token) option
  val match_error_token : token -> ErrorToken.t Error.located option
  val build_error_token : ErrorToken.t Error.located -> token

  val handle_unexpected_token :
    productions:(xsymbol * xsymbol list * production * int) list ->
    next_token:token tok ->
    acceptable_tokens:token tok list ->
    reducible_productions:production list ->
    generation_streak:int ->
    (token, production) recovery_action

  val reduce_as_parse_error : 'a -> 'a symbol -> Lexing.position -> Lexing.position -> token
  val merge_parse_error : token -> token -> token
  val is_error : 'a -> 'a symbol -> bool
  val is_eof_token : token -> bool
end

module type IncrementalParser = sig
  type ast
  type 'a checkpoint

  val main : Lexing.position -> ast checkpoint

  type token

  val token : Lexing.lexbuf -> token
end

module Make : functor
  (I : MenhirLib.IncrementalEngine.EVERYTHING)
  (M : IncrementalParser with type 'a checkpoint = 'a I.checkpoint and type token = I.token)
  (_ : Recovery
         with type token = I.token
          and type 'a symbol = 'a I.symbol
          and type xsymbol = I.xsymbol
          and type 'a terminal = 'a I.terminal
          and type 'a env = 'a I.env
          and type production = I.production)
  -> sig
  val parse : Lexing.lexbuf -> (Lexing.position * string) list * M.ast
end
