
open Lexing

module I = Parser.MenhirInterpreter

type token = Parser.token =
  | TIMES
  | THEN
  | SEMICOLON
  | RPAREN
  | PLUS
  | LPAREN
  | INT of int
  | IF
  | IDENT of string
  | FUN
  | ERROR_TOKEN of Mastic.Error.t
  | EOF
  | ELSE
  | ASSIGN
[@@deriving show]

let show_terminal : type a. a I.terminal -> string =
 fun x ->
  match x with
  | I.T_error -> "err"
  | T_TIMES -> "*"
  | T_RPAREN -> ")"
  | T_PLUS -> "+"
  | T_LPAREN -> "("
  | T_EOF -> "eof"
  | T_INT -> "int"
  | T_ERROR_TOKEN -> "perr"
  | T_THEN -> "then"
  | T_IF -> "if"
  | T_ELSE -> "else"
  | T_IDENT -> "ident"
  | T_SEMICOLON -> ";"
  | T_ASSIGN -> ":="
  | T_FUN -> "fun"

let pp_symbol : type a. a option -> Format.formatter -> a I.symbol -> unit =
 fun x fmt -> function
  | I.N I.N_func -> ( match x with None -> Format.pp_print_string fmt "<func>" | Some x -> Ast.Func.pp fmt x)
  | I.N I.N_cmd -> ( match x with None -> Format.pp_print_string fmt "<cmd>" | Some x -> Ast.Cmd.pp fmt x)
  | I.N I.N_expr -> ( match x with None -> Format.pp_print_string fmt "<expr>" | Some x -> Ast.Expr.pp fmt x)
  | I.N I.N_main -> ( match x with None -> Format.pp_print_string fmt "<main>" | Some x -> Ast.Prog.pp fmt x)
  | I.N I.N_ne_list_expr -> (
      match x with None -> Format.pp_print_string fmt "<ne list expr>" | Some x -> Ast.Expr.List.pp fmt x)
  | I.N I.N_list_func -> (
      match x with None -> Format.pp_print_string fmt "<list func>" | Some x -> Ast.Func.List.pp fmt x)
  | I.N I.N_list_cmd -> (
      match x with None -> Format.pp_print_string fmt "<list cmd>" | Some x -> Ast.Cmd.List.pp fmt x)
  | I.T I.T_INT -> ( match x with None -> Format.pp_print_string fmt "<int>" | Some x -> Format.fprintf fmt "%d" x)
  | I.T I.T_IDENT -> (
      match x with None -> Format.pp_print_string fmt "<ident>" | Some x -> Format.pp_print_string fmt x)
  | I.T T_ERROR_TOKEN -> (
      match x with None -> Format.pp_print_string fmt "perr" | Some x -> Format.fprintf fmt "%s" (Mastic.Error.show x))
  | I.T t -> Format.fprintf fmt "%s" (show_terminal t)

let token_of_terminal : type a. a I.terminal -> (string * token) option = function
  | I.T_RPAREN -> Some (")", Parser.RPAREN)
  | I.T_ASSIGN -> Some (":=", Parser.ASSIGN)
  | T_TIMES -> Some ("*", Parser.TIMES)
  | T_THEN -> Some ("then", Parser.THEN)
  | T_SEMICOLON -> Some (";", Parser.SEMICOLON)
  | T_PLUS -> Some ("+", Parser.PLUS)
  | T_LPAREN -> Some ("(", Parser.LPAREN)
  | T_INT -> None
  | T_IF -> Some ("if", Parser.IF)
  (* | T_IDENT -> None *)
  | T_IDENT -> Some ("_", Parser.IDENT "_") (* do we want this? eg: fun ( x := 1) *)
  | T_FUN -> Some ("fun", Parser.FUN)
  | T_ERROR_TOKEN -> None
  | T_EOF -> Some ("eof", Parser.EOF)
  | T_ELSE -> Some ("else", Parser.ELSE)
  | T_error -> assert false

(* -------------------------------------------------------------------------- *)

module IncrementalParser = struct
  type 'a checkpoint = 'a I.checkpoint
  type ast = Ast.Prog.t

  let main = Parser.Incremental.main

  type token = I.token

  let token = Lexer.token
end

module Strategy = struct
  type token = I.token

  let show_token = show_token

  type 'a symbol = 'a I.symbol
  type xsymbol = I.xsymbol

  let pp_symbol = pp_symbol

  type 'a terminal = 'a I.terminal
  type 'a env = 'a I.env
  type production = I.production

  let token_of_terminal = token_of_terminal
  let match_error_token = function ERROR_TOKEN x -> Some x | _ -> None
  let build_error_token t = ERROR_TOKEN t

  let is_production_for_sart_symbol = function
    | I.X (I.N I.N_main), _, _, _ -> true (* never happens *)
    | I.X (I.N I.N_list_func), _, _, _ -> true
    | _ -> false

  let is_ident x = match x.Mastic.ErrorResilientParser.t with Parser.IDENT _ -> true | _ -> false
  let is_assign x = x.Mastic.ErrorResilientParser.t = Parser.ASSIGN
  let is_function_name = function I.X (I.N I.N_func), _, _, 1 -> true | _ -> false

  let is_assign_expected (_, l, _, n) =
    match List.nth l n with I.X (I.N I.N_cmd) -> true | I.X (I.N I.N_list_cmd) -> true | _ -> false

  (* Initialize the lexer, and catch any exception raised by the lexer. *)

  (* The loop which drives the parser. At each iteration, we analyze a
   checkpoint produced by the parser, and act in an appropriate manner.
   [lexbuf] is the lexing buffer. [checkpoint] is the last checkpoint produced
   by the parser. *)

  let handle_unexpected_token ~productions ~next_token:tok ~acceptable_tokens ~reducible_productions:prods
      ~generation_streak =
    let open Mastic.ErrorResilientParser in
    if prods <> [] then Reduce (List.hd prods)
    else
      match tok with
      (* tokens that look like a good point to re-start parsing (or terminate).
       these are typically the reserved words (keywords) of the language *)
      | { t = FUN | EOF | SEMICOLON | ASSIGN | LPAREN | RPAREN | THEN | ELSE } -> begin
          match prods with
          | p :: _ ->
              (* if we can reduce we do it. it could be we are parsing something
               optional for example, but the next token is not a lookahead we exepct *)
              Reduce p
          | [] -> (
              if is_assign tok && List.exists is_assign_expected productions && generation_streak < 10 then
                GenerateToken { tok with t = Parser.IDENT "_x"; s = "_x" }
              else
                (* we try to complete one of the current productions *)
                match acceptable_tokens with
                | x :: _ ->
                    let x =
                      if is_ident x && List.exists is_function_name productions then
                        { x with t = Parser.IDENT "_f"; s = "_f" }
                      else x
                    in
                    (* this is a token that makes the automaton shift *)
                    if generation_streak < 10 then GenerateToken x else TurnIntoError
                | [] ->
                    (* if we are parsing the top level production there is not point in padding
                   TODO: maybe all list(something) should do the same *)
                    let at_start = productions |> List.exists is_production_for_sart_symbol in
                    if at_start || generation_streak >= 10 then TurnIntoError
                    else GenerateHole (* this is a hole, the error production for the current nonterminal *))
        end
      | { t = INT n } -> TurnIntoThisError (Ast.Expr.build_token (Mastic.Error.loc (Ast.Expr.Lit n) tok.b tok.e))
      | _ -> TurnIntoError

  let reduce_as_parse_error : type a. a -> a I.symbol -> position -> position -> token =
   fun x tx b e ->
    match tx with
    | I.N I.N_main -> assert false
    | I.N I.N_cmd -> ERROR_TOKEN (Ast.Cmd.build_token (Mastic.Error.loc x b e))
    | I.N I.N_expr -> ERROR_TOKEN (Ast.Expr.build_token (Mastic.Error.loc x b e))
    | I.N I.N_func -> ERROR_TOKEN (Ast.Func.build_token (Mastic.Error.loc x b e))
    | I.N I.N_list_func -> ERROR_TOKEN (Ast.Func.List.build_token (Mastic.Error.loc x b e))
    | I.N I.N_ne_list_expr -> ERROR_TOKEN (Ast.Expr.List.build_token (Mastic.Error.loc x b e))
    | I.N I.N_list_cmd -> ERROR_TOKEN (Ast.Cmd.List.build_token (Mastic.Error.loc x b e))
    | I.T y -> ERROR_TOKEN Mastic.Error.(mkLexError (loc (Format.asprintf "%a" (pp_symbol (Some x)) tx) b e))

  let is_eof_token = function EOF -> true | _ -> false
end

(* -------------------------------------------------------------------------- *)
