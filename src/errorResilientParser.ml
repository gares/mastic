let debug = ref false
module Printf = struct
include Printf
  let eprintf x = if !debug then Printf.eprintf x else Printf.ifprintf stderr x
end

  type 'token tok = string * ('token * Lexing.position * Lexing.position)

  type ('token,'production) recovery_action =
  | TurnInto of 'token tok (* the lookahead is invalid: we shift as a parse error *)
  | Generate of 'token tok (* the lookahead is invalid: we know how to complete *)
  | Reduce of 'production (* the lookahead is invalid, but the stack can be reduced *)

module type Recovery = sig
  type token
  val show_token : token -> string

  type 'a symbol
  val show_symbol : 'a option -> 'a symbol -> string

  type 'a terminal
  type 'a env
  type production
  
  val token_of_terminal : 'a terminal -> (string * token) option
  val match_error_token : token -> ErrorToken.t Error.located option


  val handle_unexpected_token : 'a env -> token tok -> token tok list -> (production * int) list -> token tok list -> production option -> int -> (token,production) recovery_action * token tok list
  val reduce_as_parse_error : 'a -> 'a symbol -> Lexing.position -> Lexing.position -> token
  val merge_parse_error : token -> token -> token
  val is_error : 'a -> 'a symbol -> bool

end

module type Main = sig
  type ast
  type 'a checkpoint
  val main: Lexing.position -> ast checkpoint
  type token
  val token : Lexing.lexbuf -> token

end
module Make(I : MenhirLib.IncrementalEngine.EVERYTHING)
           (M : Main with type 'a checkpoint = 'a I.checkpoint
                      and type token = I.token)
           (R : Recovery with type token = I.token
                          and type 'a symbol = 'a I.symbol
                          and type 'a terminal = 'a I.terminal
                          and type 'a env = 'a I.env
                          and type production = I.production) = struct

open I
open M
open R
open Lexing

let try_pop env = match pop env with Some x -> x | None -> env

type _ two_errors =
  | Empty
  | ZeroErr : token * 'x env -> 'x two_errors
  | OneErr  : token * 'x env * token * 'x env -> 'x two_errors



let two_err env =
  match top env with
  | None -> Empty
  | Some (Element(tx,x,b,e)) ->
      let tx = incoming_symbol tx in
      let ex = is_error x tx in
      if not ex then ZeroErr(reduce_as_parse_error x tx b e,try_pop env)
      else
        match pop env with
        | None -> assert false
        | Some env' ->
            match top env' with
            | Some (Element(ty,y,b1,e1)) ->
                let ty = incoming_symbol ty in
                OneErr(reduce_as_parse_error x tx b e,env',reduce_as_parse_error y ty b1 e1,try_pop env')
            | _ -> failwith "the grammar must include an atom for parse error 1"


let tail = function [] -> [] | _ :: xs -> xs
let rec drop n l = if n = 0 then l else drop (n-1) (List.tl l)

let toplevel env =
  match top env with
  | None -> None
  | Some Element(st,_,_,_) ->
      match items st with
      | [x,_] -> Some x
      | _ -> None

(* requires semantic actions to be pure *)
let ensure_reduces : type a. a env -> production -> int -> [>`Reduce of (production * int)] list = fun env prod pos ->
  try let _env : _ env = force_reduction prod env in [`Reduce(prod,pos)]
  with Invalid_argument _ -> []

let rec next_symbols env =
  match top env with 
  | None -> []
  | Some Element(st,_,_,_) ->
      let items = items st in
      List.concat_map (next_of ~last_nullable:false env) items


(* and next_symbols_opt = function None -> [] | Some env -> next_symbols env  *)
and next_of ~last_nullable env (prod,pos) =
  match drop pos (rhs prod) with
  | [] -> if last_nullable then [] else ensure_reduces env prod pos
  | (X (T _)) :: next :: [] when compare_symbols (lhs prod) next = 0 -> []
  | (X (T x)) :: _ -> token_of_terminal x |> o2l
  | (X N nt) :: _ when nullable nt -> next_of ~last_nullable:true env (prod,pos+1)
  | _ -> []

and o2l o = Option.fold ~none:[] ~some:(fun x -> [`Generate x]) o

let next_symbols env b =
  let tok (s,t) = s,(t,b,b) in
  let l = next_symbols env in
  let gen = List.filter_map (function `Generate x -> Some (tok x) | _ -> None) l in
  let red = List.filter_map (function `Reduce x -> Some x | _ -> None) l in
  gen, red

let valid t =
  match match_error_token t with
  | None -> assert false
  | Some x -> t, Error.bloc x, Error.eloc x
  
let pi1 (x,_,_) = x
let pi2 (_,x,_) = x

let is_error_token x = match match_error_token x with None -> false | _ -> true

let show_element = function (Element(st,x,_,_)) ->
  show_symbol (Some x) @@ incoming_symbol st

let show_env env = 
  let rec to_list env =
    match top env with
    | None -> []
    | Some x -> x :: match pop env with None -> [] | Some x -> to_list x in
  let stack = List.rev @@ to_list env in
  String.concat "; " (List.map show_element stack)

let show_gens l = List.map fst l |> String.concat " "
let show_xsymbol = function X s -> show_symbol None s
let show_prod (x,n) =
  Printf.sprintf "[%s]%s"
    (String.concat " " (List.map show_xsymbol (rhs x)))
    (if n = -1 then "" else "@" ^ string_of_int n)
let show_prods l = String.concat " " (List.map show_prod l)

type state = {
  lexbuf : lexbuf;
  errbuf : (position * string) list;
  incoming_toks : token tok list;
  generation_streak : int;
}

let rec loop st (checkpoint : ast checkpoint) =
  match checkpoint with
  | InputNeeded _env ->
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         checkpoint. Then, repeat. *)
      let token, st =
        match st.incoming_toks with
        | t :: _ -> snd t, st
        | [] ->
            let tok = token st.lexbuf in
            let toks = Lexing.lexeme st.lexbuf in
            let toks = if toks = "" then show_token tok else toks in
            let startp = st.lexbuf.lex_start_p
            and endp = st.lexbuf.lex_curr_p in
            let tok = tok, startp, endp in
            let last_tok = toks, tok in
            tok, { st with incoming_toks = [last_tok]; generation_streak = 0 } in
      Printf.eprintf "READ %s\n" (show_token @@ pi1 token);
      let checkpoint = offer checkpoint token in
      loop st checkpoint
  | Shifting (_,s,_) ->
      Printf.eprintf "SHIFT [%s]\n" (show_env s);
      let checkpoint = resume checkpoint in
      loop { st with incoming_toks = tail st.incoming_toks } checkpoint
  | AboutToReduce (s,p) ->
      let n = List.length @@ rhs p in
      Printf.eprintf "RED %d [%s]\n" n (show_env s);
      let checkpoint = resume checkpoint in
      loop st checkpoint
  | Accepted v -> st.errbuf, v
      (* The parser has succeeded and produced a semantic value. Print it. *)
  | Rejected ->
      (* The parser rejects this input. This cannot happen, here, because
         we stop as soon as the parser reports [HandlingError]. *)
      assert false
  | HandlingError env ->
      Printf.eprintf "* ERROR: stack [%s]\n" (show_env env);
      match st.incoming_toks with
      | (s,(t,b,e)) :: incoming_toks when is_error_token t ->
          Printf.eprintf "  LOOKAHEAD: %s\n" (show_token t);
          (* b and e are likely wrong after merge *)
          begin match two_err env with
          | Empty -> failwith "the grammar start symbol must include an atom for parse error" 
          | ZeroErr(t0,env) ->
             let t = merge_parse_error t0 t in
             Printf.eprintf "  RECOVERY: push (squashed) %s on [%s]\n" (show_token t) (show_env env);
             let checkpoint = offer (input_needed env) (valid t) in
             let incoming_toks =  (s,(t,b,e)) :: incoming_toks in
             loop { st with incoming_toks } checkpoint
          | OneErr(t0,env,_,_) -> (* TODO: factor with prev case *)
             let t = merge_parse_error t0 t in
             Printf.eprintf "  RECOVERY: push (squashed) %s on [%s]\n" (show_token t) (show_env env);
             let checkpoint = offer (input_needed env) (valid t) in
             let incoming_toks =  (s,(t,b,e)) :: incoming_toks in
             loop { st with incoming_toks } checkpoint
          end
      | tok :: incoming_toks ->
          Printf.eprintf "  LOOKAHEAD: %s\n" (show_token (snd tok |> pi1));
          let gens, prods = next_symbols env (snd tok |> pi2) in
          let toplevel = toplevel env in
          Printf.eprintf "    PROPOSE: reductions: %s\n" (show_prods prods);
          Printf.eprintf "    PROPOSE: tokens: %s\n"  (show_gens gens);
          Printf.eprintf "    PROPOSE: current production: %s\n"  (toplevel |> Option.fold ~none:"none" ~some:(fun x -> show_prod (x,-1)));
          begin match handle_unexpected_token env tok incoming_toks prods gens toplevel st.generation_streak with
          | TurnInto t, incoming_toks ->
              Printf.eprintf "  RECOVERY: turn to %s and push\n" (show_token (snd t |> pi1));
              let checkpoint = offer (input_needed env) (snd t) in
              loop { st with incoming_toks } checkpoint
          | Generate t, incoming_toks ->
              Printf.eprintf "  RECOVERY: generate %s and push\n" (fst t);
              let checkpoint = offer (input_needed env) (snd t) in
              let errbuf = (snd t |> pi2,fst t) :: st.errbuf in
              let generation_streak = st.generation_streak + 1 in
              loop { st with incoming_toks; errbuf; generation_streak } checkpoint
          | Reduce p, incoming_toks ->
              Printf.eprintf "  RECOVERY: reduce %s\n" (show_prod (p,-1));
              let checkpoint = input_needed (force_reduction p env) in
              loop { st with incoming_toks } checkpoint
          end
      | [] ->
          Printf.eprintf "  STUCK\n";
          match two_err env with
          | Empty -> assert false
          | ZeroErr(t,env) ->
             Printf.eprintf "  RECOVERY: push %s on [%s]\n" (show_token t) (show_env env);
             (* loop? *)
             let checkpoint = offer (input_needed env) (valid t) in
             loop st checkpoint
          | OneErr(t0,_,t,env) ->
              let t = merge_parse_error t0 t in
              Printf.eprintf "  RECOVERY: push (squashed) %s on [%s]\n" (show_token t) (show_env env);
              let checkpoint = offer (input_needed env) (valid t) in
              loop st checkpoint


let parse lexbuf =
    let checkpoint = main lexbuf.lex_curr_p in
    let st =  { lexbuf; errbuf = []; generation_streak = 0; incoming_toks = [] } in
    loop st checkpoint


end