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
  (** for debugging *)

  type 'a symbol
  type xsymbol

  val pp_symbol : 'a option -> Format.formatter -> 'a symbol -> unit
  (** for debugging *)

  type 'a terminal
  type 'a env
  type production

  val match_error_token : token -> Error.t option
  (** identify the [ERROR_TOKEN] *)

  val build_error_token : Error.t -> token
  (** build the [ERROR_TOKEN] *)

  val is_eof_token : token -> bool
  (** identify the [EOF] token *)

  val handle_unexpected_token :
    productions:(xsymbol * xsymbol list * production * int) list ->
    next_token:token tok ->
    acceptable_tokens:token tok list ->
    reducible_productions:production list ->
    generation_streak:int ->
    (token, production) recovery_action
  (** called when [next_token] does not fit *)

  val token_of_terminal : 'a terminal -> (string * token) option
  (** used to generate [~acceptable_tokens] for [handle_unexpected_token] *)

  val reduce_as_parse_error : 'a -> 'a symbol -> Lexing.position -> Lexing.position -> token
  (** store in the error token an ast using the [build_token] api, eg
      [ERROR_TOKEN (Ast.Expr.build_token (Mastic.Error.loc x b e))] *)
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
