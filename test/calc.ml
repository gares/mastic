

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
    | ERROR_TOKEN of Mastic.ErrorToken.t Mastic.Error.located
    | EOF
    | ELSE
    | ASSIGN
[@@deriving show]

let show_terminal : type a. a I.terminal -> string = fun x -> match x with
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

let show_symbol : type a. a option -> a I.symbol -> string = fun x -> function
  | I.N I.N_func -> x |> Option.fold ~none:"<func>" ~some:Ast.Func.show 
  | I.N I.N_cmd  -> x |> Option.fold ~none:"<cmd>"  ~some:Ast.Cmd.show  
  | I.N I.N_expr -> x |> Option.fold ~none:"<expr>" ~some:Ast.Expr.show 
  | I.N I.N_main -> x |> Option.fold ~none:"<main>" ~some:Ast.Prog.show 

  | I.N I.N_list_expr_ -> x |> Option.fold ~none:"<list expr>" ~some:(fun x -> String.concat " " @@ List.map Ast.Expr.show x)
  | I.N I.N_list_func_ -> x |> Option.fold ~none:"<list func>" ~some:(fun x -> String.concat " " @@ List.map Ast.Func.show x)
  
  | I.N I.N_loption_separated_nonempty_list_SEMICOLON_cmd__ -> x |> Option.fold ~none:"<opt_nelist cmd>" ~some:(fun x -> String.concat " " @@ List.map Ast.Cmd.show x)
  | I.N I.N_separated_nonempty_list_SEMICOLON_cmd_          -> x |> Option.fold ~none:"<nelist cmd>" ~some:(fun x -> String.concat " " @@ List.map Ast.Cmd.show x)

  | I.T I.T_INT  -> x |> Option.fold ~none:"<int>" ~some:string_of_int
  | I.T I.T_IDENT  -> x |> Option.fold ~none:"<ident>" ~some:(fun x -> x)
  | I.T t -> show_terminal t



(* -------------------------------------------------------------------------- *)


module Main = struct
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
  let show_symbol = show_symbol

  type 'a terminal = 'a I.terminal
  type 'a env = 'a I.env
  type production = I.production

  let token_of_terminal : type a.a terminal -> (string * token) option = function
  | I.T_RPAREN  -> Some (")",Parser.RPAREN)
  | I.T_ASSIGN  -> Some (":=",Parser.ASSIGN)
  | T_TIMES -> Some("*",Parser.TIMES)
  | T_THEN -> Some("then",Parser.THEN)
  | T_SEMICOLON -> Some(";",Parser.SEMICOLON)
  | T_PLUS -> Some("+",Parser.PLUS)
  | T_LPAREN -> Some("(",Parser.LPAREN)
  | T_INT -> Some("int",Parser.INT 0)
  | T_IF -> Some("if",Parser.IF)
  | T_IDENT -> Some("ident",Parser.IDENT "_")
  | T_FUN -> Some("fun",Parser.FUN)
  | T_ERROR_TOKEN -> None
  | T_EOF -> Some("eof",Parser.EOF)
  | T_ELSE -> Some("else",Parser.ELSE)
  | _ -> None

  let match_error_token = function
  | ERROR_TOKEN x -> Some x
  | _ -> None
let hacks = ref 0
let hack () =
  decr hacks; if !hacks < 0 then failwith "Injecting too many tokens"

(* let rec filter_out_infix = function
  | (_,(Parser.(TIMES | PLUS),_,_)) :: xs -> filter_out_infix xs
  | x :: xs -> x :: filter_out_infix xs
  | [] -> [] *)

(* Initialize the lexer, and catch any exception raised by the lexer. *)
let handle_unexpected_token _env tok toks prods next current_production generation_streak =
  let open Mastic.ErrorResilientParser in
  let to_error (s,(_,b,e)) = 
    let next_tok = s, (ERROR_TOKEN (Mastic.ErrorToken.mkLexError s b e),b,e) in
    TurnInto next_tok, next_tok :: toks in
  let generate_dummy (_,(_,b,_e))  =
     hack ();
     let next_tok = "_", (ERROR_TOKEN (Mastic.ErrorToken.mkLexError "_" b b),b,b) in
     Generate next_tok, next_tok :: tok :: toks in
  match tok with
  | _,(Parser.(FUN | EOF | SEMICOLON | RPAREN | THEN | ELSE),b,e) ->
        begin match prods with
        | (p,_) :: _ -> Reduce p, tok :: toks
        | [] ->
            match next with
            | (s,(c,_,_)) :: _ when generation_streak < 10 ->
             let next_tok = s, (c,b,e) in
             Generate next_tok, next_tok :: tok :: toks
            | _ ->
              match current_production with
              (* | Some p when I.lhs p = I.X (I.N I.N_main) -> to_error tok *)
              | Some p when I.lhs p = I.X (I.N I.N_list_func_) -> to_error tok (* do not generate toplevel items *)
              | _ -> generate_dummy tok
        end
  | _ -> to_error tok


let reduce_as_parse_error : type a. a -> a I.symbol -> position -> position -> token = fun x tx b e -> match tx with
  | I.N I.N_main -> assert false
  | I.N I.N_loption_separated_nonempty_list_SEMICOLON_cmd__ -> assert false
  | I.N I.N_separated_nonempty_list_SEMICOLON_cmd_ -> assert false
  | I.N I.N_cmd -> ERROR_TOKEN (Mastic.Error.loc (Ast.Cmd.Cmd x) b e)
  | I.N I.N_func -> assert false
  | I.N I.N_list_func_ -> assert false
  | I.N I.N_expr -> ERROR_TOKEN (Mastic.Error.loc (Ast.Expr.Expr x) b e)
  | I.N I.N_list_expr_ ->  assert false
    (* ERROR_TOKEN (Ast.Expr.ERROR_TOKEN([],[x,b,e])),b,e *)
  | I.T I.T_INT  -> ERROR_TOKEN (Mastic.ErrorToken.mkLexError (string_of_int x) b e)
  | I.T I.(T_ERROR_TOKEN) -> assert false
  | I.T I.T_IDENT -> ERROR_TOKEN (Mastic.ErrorToken.mkLexError x b e)
  | I.T y -> ERROR_TOKEN (Mastic.ErrorToken.mkLexError (show_terminal y) b e)

let is_error : type a. a -> a I.symbol -> bool = fun x tx -> match tx with
  | I.N I.N_main -> Ast.Prog.is_err x
  | I.N I.N_cmd  -> Ast.Cmd.is_err x 
  | I.N I.N_func -> Ast.Func.is_err x
  | I.N I.N_expr -> Ast.Expr.is_err x
  | _ -> false
let  merge_parse_error x y =
  let open Mastic in
  let open Parser in
  match x, y with
  | ERROR_TOKEN x, ERROR_TOKEN y -> ERROR_TOKEN (ErrorToken.merge x y)
  | _ -> assert false
  
end

(* -------------------------------------------------------------------------- *)

module ERParser = Mastic.ErrorResilientParser.Make(I)(Main)(Recovery)


let included_opt f x y =
  match x, y with
  | None, None -> true
  | Some x, Some y -> f x y
  | _ -> false
let rec included_list f e l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | x::xs, y::ys -> (f x y && included_list f e xs ys) ||
                    (e x && included_list f e xs l2)
  | _ -> false

let rec included_prog : Ast.Prog.t -> Ast.Prog.t -> bool = fun x y ->
  let open Ast.Prog in
  match x, y with
  | Err _, _ -> true
  | P x, P y -> included_list included_fun Ast.Func.is_err x y
  | _ -> false
and included_fun x y =
  let open Ast.Func in
  match x, y with
  | Err _, _ -> true
  | Fun(n1,l1), Fun(n2,l2) -> n1 = n2 && included_list included_cmd Ast.Cmd.is_err l1 l2
  | _ -> false
and included_cmd x y =
  let open Ast.Cmd in
  match x,y with
  | Err _, _ -> true
  | Assign(n1,v1), Assign(n2,v2) -> n1 = n2 && included_expr v1 v2
  | If(v1,x1,oy1), If(v2,x2,oy2) -> included_expr v1 v2 && included_cmd x1 x2 && included_opt included_cmd oy1 oy2
  | _ -> false
and included_expr x y =
  let open Ast.Expr in
  match x,y with
  | Err _, _ -> true
  | Lit n, Lit m -> n = m
  | Mul(x1,y1), Mul(x2,y2) -> included_expr x1 y1 && included_expr x2 y2
  | Add(x1,y1), Add(x2,y2) -> included_expr x1 y1 && included_expr x2 y2
  | Call(f,xs), Call (g,ys) -> f = g && included_list included_expr is_err xs ys
  | _ -> false


let underline l (b,e) =
  Bytes.iteri (fun i _ -> if b.pos_cnum <= i && i <= e.pos_cnum then Bytes.set l i '^' else ()) l

let rec on_prog f = function
  | Ast.Prog.P l -> List.iter (on_func f) l
  | Err e -> f (Mastic.Error.span e)
and on_func f = function
  | Ast.Func.Fun(_,l) -> List.iter (on_cmd f) l
  | Err e -> f (Mastic.Error.span e)
and on_cmd f = function
  | Ast.Cmd.Assign(_,v) -> on_expr f v
  | Ast.Cmd.If(v,t,None) -> on_expr f v; on_cmd f t
  | Ast.Cmd.If(v,t,Some e) -> on_expr f v; on_cmd f t; on_cmd f e
  | Err e -> f (Mastic.Error.span e)
and on_expr f = function
  | Ast.Expr.Add(e1,e2) -> on_expr f e1; on_expr f e2
  | Ast.Expr.Mul(e1,e2) -> on_expr f e1; on_expr f e2
  | Ast.Expr.Call(_,l) -> List.iter (on_expr f) l
  | Ast.Expr.Lit _ -> ()
  | Err e -> f (Mastic.Error.span e)

let show_result header line errbuf v =
    let my_header = "error: " in
    let padding = String.length header - String.length my_header in
    let padding = String.make padding ' ' in
    let b = String.to_bytes (String.make (String.length line) ' ') in
    on_prog (underline b) v;
    let b = String.of_bytes b in
    if String.index_opt b '^' <> None then Printf.printf "%s%s%s recovered syntax error\n" my_header padding b;
    List.rev errbuf |> List.iter (fun (p,s) ->
      let col = p.Lexing.pos_cnum in
      let msg = String.make col ' ' ^ "^ completed with " ^ s in
      Printf.printf "%s%s%s\n" my_header padding msg);
    Printf.printf "ast: %s\n" (Ast.Prog.show v);
    flush_all ()

let fuzz_with = [|';';' ';'$'|]
let fuzz_with n = fuzz_with.(n mod Array.length fuzz_with)

let fuzz l =
  let n = String.length l in
  let m = Random.int (n-1) in
  String.mapi (fun i c -> if i = m then fuzz_with m else c) l

let fuzz_no = ref 0
let only_fno = ref 0



let process (line : string) =
  let line = List.hd @@ String.split_on_char '\n' line in
  try
    Recovery.hacks := String.length line;
    let header = "input: " in
    Printf.printf "%s%s\n" header line;
    let lexbuf = from_string line in
    let errbuf, v = ERParser.parse lexbuf in
    show_result header line errbuf v;
    Printf.printf "\n";
    for i = 1 to !fuzz_no do
      let line = fuzz line in
      if !only_fno < 1 || i = !only_fno then begin
        let header = Printf.sprintf "fuzzed input #%d: " i in
        Printf.printf "%s%s\n" header line;
        let lexbuf = from_string line in
        Recovery.hacks := String.length line;
        let errbuf', v' = ERParser.parse lexbuf in
        show_result header line errbuf' v';
        if not (included_prog v' v) then
           Printf.printf "note: not a subterm\n";
        Printf.printf "\n"
      end
    done
  with
  | Lexer.Error msg ->
      Printf.printf "error: %s%!" msg; exit 1

(* -------------------------------------------------------------------------- *)

(* The rest of the code is as in the [calc] demo. *)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let main channel =
  (* Attempt to read one line. *)
  let optional_line, _ = Lexer.line channel in
  process optional_line

let () =
  let file = ref stdin in
  let seed = ref 0 in
  Arg.parse [
    ("-fuzz",Arg.Set_int fuzz_no,"how many fuzz (default 0)");
    ("-only",Arg.Set_int only_fno,"only run fuzz number N");
    ("-seed",Arg.Set_int seed,"random seed");
    ("-debug",Arg.Set Mastic.ErrorResilientParser.debug,"verbose")
  ]
    (fun f -> file := open_in f) "help";
  Random.init !seed;
  main (from_channel stdin)


