open Lexing

(* A short name for the incremental parser API. *)

module I = Parser.MenhirInterpreter

(* -------------------------------------------------------------------------- *)

(* The loop which drives the parser. At each iteration, we analyze a
   checkpoint produced by the parser, and act in an appropriate manner.
   [lexbuf] is the lexing buffer. [checkpoint] is the last checkpoint produced
   by the parser. *)

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

let show_symbol : type a. a option -> a I.symbol -> string =
 fun x -> function
  | I.N I.N_func -> x |> Option.fold ~none:"<func>" ~some:Ast.Func.show
  | I.N I.N_cmd -> x |> Option.fold ~none:"<cmd>" ~some:Ast.Cmd.show
  | I.N I.N_expr -> x |> Option.fold ~none:"<expr>" ~some:Ast.Expr.show
  | I.N I.N_main -> x |> Option.fold ~none:"<main>" ~some:Ast.Prog.show
  | I.N I.N_ne_list_expr -> x |> Option.fold ~none:"<ne list expr>" ~some:(fun x -> Ast.Expr.List.show x)
  | I.N I.N_list_func -> x |> Option.fold ~none:"<list func>" ~some:(fun x -> Ast.Func.List.show x)
  | I.N I.N_list_cmd -> x |> Option.fold ~none:"<list cmd>" ~some:(fun x -> Ast.Cmd.List.show x)
  | I.T I.T_INT -> x |> Option.fold ~none:"<int>" ~some:string_of_int
  | I.T I.T_IDENT -> x |> Option.fold ~none:"<ident>" ~some:(fun x -> x)
  | I.T t -> show_terminal t

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
  | T_IDENT -> None
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

module Recovery = struct
  type token = I.token

  let show_token = show_token

  type 'a symbol = 'a I.symbol
  type xsymbol = I.xsymbol

  let show_symbol = show_symbol

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

  (* Initialize the lexer, and catch any exception raised by the lexer. *)
  let handle_unexpected_token ~productions ~next_token:tok ~acceptable_tokens ~reducible_productions:prods
      ~generation_streak =
    let open Mastic.ErrorResilientParser in
    match tok with
    (* tokens that look like a good point to re-start parsing (or terminate).
       these are typically the reserved words (keywords) of the language *)
    | { t = FUN | EOF | SEMICOLON | RPAREN | THEN | ELSE } -> begin
        match prods with
        | p :: _ ->
            (* if we can reduce we do it. it could be we are parsing something
               optional for example, but the next token is not a lookahead we exepct *)
            Reduce p
        | [] -> (
            (* we try to complete one of the current productions *)
            match acceptable_tokens with
            | x :: _ ->
                (* this is a token that makes the automaton shift *)
                if generation_streak < 10 then GenerateToken x else TurnIntoError
            | [] ->
                (* if we are parsing the top level production there is not point in padding
                   TODO: maybe all list(something) should do the same *)
                let at_start = productions |> List.exists is_production_for_sart_symbol in
                if at_start || generation_streak >= 10 then TurnIntoError
                else GenerateHole (* this is a hole, the error production for the current nonterminal *))
      end
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
    | I.T y -> ERROR_TOKEN Mastic.Error.(mkLexError (loc (show_symbol (Some x) tx) b e))

  let is_error : type a. a -> a I.symbol -> bool =
   fun x tx ->
    match tx with
    | I.N I.N_main -> Ast.Prog.is_err x
    | I.N I.N_cmd -> Ast.Cmd.is_err x
    | I.N I.N_func -> Ast.Func.is_err x
    | I.N I.N_expr -> Ast.Expr.is_err x
    | _ -> false

  let merge_parse_error x y =
    let open Mastic in
    let open Parser in
    match (x, y) with ERROR_TOKEN x, ERROR_TOKEN y -> ERROR_TOKEN (Error.merge x y) | _ -> assert false

  let is_eof_token = function EOF -> true | _ -> false
end

(* -------------------------------------------------------------------------- *)

module ERParser = Mastic.ErrorResilientParser.Make (I) (IncrementalParser) (Recovery)

let included_opt f x y = match (x, y) with None, None -> true | Some x, Some y -> f x y | _ -> false

let rec included_prog : Ast.Prog.t -> Ast.Prog.t -> bool =
 fun x y ->
  let open Ast.Prog in
  match (x, y) with
  | Err _, _ -> true
  | P x, P y -> Mastic.ErrorList.included included_fun Ast.Func.is_err x y
  | _ -> false

and included_fun : Ast.Func.t -> Ast.Func.t -> bool =
 fun x y ->
  let open Ast.Func in
  match (x, y) with
  | Err _, _ -> true
  | Fun (n1, l1), Fun (n2, l2) -> n1 = n2 && Mastic.ErrorList.included included_cmd Ast.Cmd.is_err l1 l2
  | _ -> false

and included_cmd (x : Ast.Cmd.t) y =
  let open Ast.Cmd in
  match (x, y) with
  | Err _, _ -> true
  | Assign (n1, v1), Assign (n2, v2) -> n1 = n2 && included_expr v1 v2
  | If (v1, x1, oy1), If (v2, x2, oy2) -> included_expr v1 v2 && included_cmd x1 x2 && included_opt included_cmd oy1 oy2
  | _ -> false

and included_expr x y =
  let open Ast.Expr in
  match (x, y) with
  | Err _, _ -> true
  | Lit n, Lit m -> n = m
  | Mul (x1, y1), Mul (x2, y2) -> included_expr x1 y1 && included_expr x2 y2
  | Add (x1, y1), Add (x2, y2) -> included_expr x1 y1 && included_expr x2 y2
  | Call (f, xs), Call (g, ys) -> f = g && Mastic.ErrorList.included included_expr is_err xs ys
  | _ -> false

let rec iter_err b e f = function
  | Mastic.Error.Lex s as x -> f x b e
  | Ast.Expr.Expr e -> iter_expr f e
  | Ast.Expr.List.List e -> Ast.Expr.List.iter (iter_expr f) (iter_loc f) e
  | Ast.Cmd.Cmd e -> iter_cmd f e
  | Ast.Cmd.List.List e -> Ast.Cmd.List.iter (iter_cmd f) (iter_loc f) e
  | Ast.Prog.Prog e -> iter_prog f e
  | Ast.Func.Func e -> iter_func f e
  | Ast.Func.List.List e -> Ast.Func.List.iter (iter_func f) (iter_loc f) e
  | _ -> ()

and iter_loc f x =
  let b = Mastic.Error.bloc x in
  let e = Mastic.Error.eloc x in
  iter_err b e f (Mastic.Error.unloc x)

and iter_expr f = function
  | Ast.Expr.Add (e1, e2) ->
      iter_expr f e1;
      iter_expr f e2
  | Ast.Expr.Mul (e1, e2) ->
      iter_expr f e1;
      iter_expr f e2
  | Ast.Expr.Call (_, l) -> Ast.Expr.List.iter (iter_expr f) (iter_loc f) l
  | Ast.Expr.Lit _ -> ()
  | Err e -> List.iter (iter_loc f) e

and iter_func f = function
  | Ast.Func.Fun (_, l) -> Ast.Cmd.List.iter (iter_cmd f) (iter_loc f) l
  | Ast.Func.Err e -> List.iter (iter_loc f) e

and iter_cmd f = function
  | Ast.Cmd.Assign (_, v) -> iter_expr f v
  | Ast.Cmd.If (v, t, None) ->
      iter_expr f v;
      iter_cmd f t
  | Ast.Cmd.If (v, t, Some e) ->
      iter_expr f v;
      iter_cmd f t;
      iter_cmd f e
  | Err e -> List.iter (iter_loc f) e

and iter_prog f = function
  | Ast.Prog.P l -> Ast.Func.List.iter (iter_func f) (iter_loc f) l
  | Ast.Prog.Err e -> List.iter (iter_loc f) e

let underline l _ b e = Bytes.iteri (fun i _ -> if b.pos_cnum <= i && i < e.pos_cnum then Bytes.set l i '^' else ()) l
let write_lex_err l x b e =
  match x with
  | Mastic.Error.Lex s -> Bytes.iteri (fun i _ -> if b.pos_cnum <= i && i < e.pos_cnum then Bytes.set l i s.[i-b.pos_cnum] else ()) l
  | _ -> ()

let show_result header line errbuf v =
  let my_header = "error: " in
  let padding = String.length header - String.length my_header in
  let padding = String.make padding ' ' in

  let white = String.make (String.length line) ' ' in

  let b = String.to_bytes white in
  iter_prog (underline b) v;
  let b = String.of_bytes b in

  let c = String.to_bytes white in
  iter_prog (write_lex_err c) v;
  let c = String.of_bytes c in

  if b <> white then Printf.printf "%s%s%s recovered syntax error\n" my_header padding b;
  if c <> white then Printf.printf "%s%s%s lex errors\n" my_header padding c;

  List.rev errbuf
  |> List.iter (fun (p, s) ->
         let col = p.Lexing.pos_cnum in
         let msg = String.make col ' ' ^ "^ completed with " ^ s in
         Printf.printf "%s%s%s\n" my_header padding msg);
  Printf.printf "ast: %s\n" (Ast.Prog.show v);
  flush_all ()

let fuzz_with = [| ';'; ' '; '$' |]
let fuzz_with n = fuzz_with.(n mod Array.length fuzz_with)

let fuzz rands l =
  let m = List.hd !rands in
  rands := List.tl !rands;
  String.mapi (fun i c -> if i = m then fuzz_with m else c) l

let fuzz_no = ref 0
let only_fno = ref 0

let process rands (line : string) =
  let line = List.hd @@ String.split_on_char '\n' line in
  let header = "input: " in
  Printf.printf "%s%s\n" header line;
  let lexbuf = from_string line in
  let errbuf, v = ERParser.parse lexbuf in
  show_result header line errbuf v;
  Printf.printf "\n";
  for i = 1 to !fuzz_no do
    let line = fuzz rands line in
    if !only_fno < 1 || i = !only_fno then begin
      let header = Printf.sprintf "fuzzed input #%d: " i in
      Printf.printf "%s%s\n%!" header line;
      let lexbuf = from_string line in
      let errbuf', v' = ERParser.parse lexbuf in
      show_result header line errbuf' v';
      if not (included_prog v' v) then Printf.printf "note: not a subterm\n";
      Printf.printf "\n"
    end
  done

(* -------------------------------------------------------------------------- *)

(* The rest of the code is as in the [main] demo. *)

let rec draw_rands user_given how_many bound =
  if how_many = 0 then []
  else
    let r, user_given =
      match user_given with
      | [] -> (Random.int (bound - 1), user_given)
      | x :: user_given -> if x >= bound then exit 2 else (x, user_given)
    in
    r :: draw_rands user_given (how_many - 1) bound

let process rands line =
  let rands = draw_rands rands !fuzz_no (String.length line) in
  Printf.printf "random: %s\n" (String.concat "," (List.map string_of_int rands));
  process (ref rands) line

let main rands channel =
  let line = input_line channel in
  process rands line

let () =
  let file = ref stdin in
  let rands = ref "" in
  Arg.parse
    [
      ("-fuzz", Arg.Set_int fuzz_no, "how many fuzz (default 0)");
      ("-only", Arg.Set_int only_fno, "only run fuzz number N");
      ("-rands", Arg.Set_string rands, "random values (comma separated)");
      ("-debug", Arg.Set Mastic.ErrorResilientParser.debug, "verbose");
    ]
    (fun f -> file := open_in f)
    "help";
  let rands =
    String.split_on_char ',' !rands
    |> List.map (fun x -> try Some (int_of_string x) with _ -> None)
    |> List.filter_map (fun x -> x)
  in
  main rands !file
