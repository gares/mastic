let debug = ref false
let dbg f = if !debug then f () else ()
let say = Printf.eprintf

type 'token tok = string * ('token * Lexing.position * Lexing.position)

type ('token, 'production) recovery_action =
  | TurnInto of 'token tok (* the lookahead is invalid: we shift as a parse error *)
  | Generate of 'token tok (* the lookahead is invalid: we know how to complete *)
  | Reduce of 'production (* the lookahead is invalid, but the stack can be reduced *)

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

  val handle_unexpected_token :
    productions:(xsymbol * xsymbol list * production * int) list ->
    next_token:token tok ->
    more_tokens:token tok list ->
    acceptable_tokens:token tok list ->
    reducible_productions:production list ->
    generation_streak:int ->
    (token, production) recovery_action * token tok list

  val reduce_as_parse_error : 'a -> 'a symbol -> Lexing.position -> Lexing.position -> token
  val merge_parse_error : token -> token -> token
  val is_error : 'a -> 'a symbol -> bool
end

module type Main = sig
  type ast
  type 'a checkpoint

  val main : Lexing.position -> ast checkpoint

  type token

  val token : Lexing.lexbuf -> token
end

module Make
    (I : MenhirLib.IncrementalEngine.EVERYTHING)
    (M : Main with type 'a checkpoint = 'a I.checkpoint and type token = I.token)
    (R :
      Recovery
        with type token = I.token
         and type 'a symbol = 'a I.symbol
         and type xsymbol = I.xsymbol
         and type 'a terminal = 'a I.terminal
         and type 'a env = 'a I.env
         and type production = I.production) =
struct
  open I
  open M
  open R
  open Lexing

  let try_pop env = match pop env with Some x -> x | None -> env

  type _ two_errors =
    | Empty
    | ZeroErr : token * 'x env -> 'x two_errors
    | OneErr : token * 'x env * token * 'x env -> 'x two_errors

  let two_err env =
    match top env with
    | None -> Empty
    | Some (Element (tx, x, b, e)) -> (
        let tx = incoming_symbol tx in
        let ex = is_error x tx in
        if not ex then ZeroErr (reduce_as_parse_error x tx b e, try_pop env)
        else
          match pop env with
          | None -> assert false
          | Some env' -> (
              match top env' with
              | Some (Element (ty, y, b1, e1)) ->
                  let ty = incoming_symbol ty in
                  OneErr (reduce_as_parse_error x tx b e, env', reduce_as_parse_error y ty b1 e1, try_pop env')
              | _ -> failwith "the grammar must include an atom for parse error 1"))

  let tail = function [] -> [] | _ :: xs -> xs
  let rec drop n l = if n = 0 then l else drop (n - 1) (List.tl l)

  let automaton_productions env =
    match top env with
    | None -> []
    | Some (Element (st, _, _, _)) -> items st |> List.map (fun (p, i) -> (lhs p, rhs p, p, i))

  (* requires semantic actions to be pure *)
  let ensure_reduces : type a. a env -> production -> int -> [> `Reduce of production ] list =
   fun env prod pos ->
    try
      let _env : _ env = force_reduction prod env in
      [ `Reduce prod ]
    with Invalid_argument _ -> []

  let rec next_symbols env =
    match top env with
    | None -> []
    | Some (Element (st, _, _, _)) ->
        let items = items st in
        List.concat_map (next_of ~last_nullable:false env) items

  (* and next_symbols_opt = function None -> [] | Some env -> next_symbols env  *)
  and next_of ~last_nullable env (prod, pos) =
    match drop pos (rhs prod) with
    | [] -> if last_nullable then [] else ensure_reduces env prod pos
    | [ X (T _); next ] when compare_symbols (lhs prod) next = 0 -> []
    | X (T x) :: _ -> token_of_terminal x |> o2l
    | X (N nt) :: _ when nullable nt -> next_of ~last_nullable:true env (prod, pos + 1)
    | _ -> []

  and o2l o = Option.fold ~none:[] ~some:(fun x -> [ `Generate x ]) o

  let automaton_possible_moves env b =
    let tok (s, t) = (s, (t, b, b)) in
    let l = next_symbols env in
    let gen = List.filter_map (function `Generate x -> Some (tok x) | _ -> None) l in
    let red = List.filter_map (function `Reduce x -> Some x | _ -> None) l in
    (gen, red)

  let valid t = match match_error_token t with None -> assert false | Some x -> (t, Error.bloc x, Error.eloc x)
  let pi1 (x, _, _) = x
  let pi2 (_, x, _) = x
  let is_error_token x = match match_error_token x with None -> false | _ -> true
  let show_element elt = match elt with Element (st, x, _, _) -> show_symbol (Some x) @@ incoming_symbol st

  let show_env env =
    let rec to_list env =
      match top env with None -> [] | Some x -> x :: (match pop env with None -> [] | Some x -> to_list x)
    in
    let stack = List.rev @@ to_list env in
    String.concat "; " (List.map show_element stack)

  let show_gens l = List.map fst l |> String.concat " "
  let show_xsymbol = function X s -> show_symbol None s
  let show_prod x = Printf.sprintf "[%s]" (String.concat " " (List.map show_xsymbol (rhs x)))
  let show_prods l = String.concat " " (List.map show_prod l)

  type state = {
    lexbuf : lexbuf; (* the stream of tokens *)
    errbuf : (position * string) list; (* all tokens inserted *)
    incoming_toks : token tok list; (* the head of the token is the lookahead *)
    generation_streak : int; (* how many dummy tokens were generated since the last read from the stream *)
  }

  let rec loop st (ckpt : ast checkpoint) =
    match ckpt with
    (* pretty much the definition of error resiliency *)
    | Rejected -> assert false
    (* standard part, we just log what happend for debugging. Shifting pops the tokens buffer *)
    | Accepted v ->
        dbg (fun () -> say "ACCEPT\n");
        (st.errbuf, v)
    | Shifting (_, s, _) ->
        dbg (fun () -> say "SHIFT [%s]\n" (show_env s));
        let chkp = resume ckpt in
        loop { st with incoming_toks = tail st.incoming_toks } chkp
    | AboutToReduce (s, p) ->
        let n = List.length @@ rhs p in
        dbg (fun () -> say "RED %d [%s]\n" n (show_env s));
        let chkp = resume ckpt in
        loop st chkp
    (* reading: we save the token into a buffer when empty, and read from the buffer if not empty *)
    | InputNeeded _env ->
        let token, st =
          match st.incoming_toks with
          | t :: _ -> (snd t, st)
          | [] ->
              let tok = token st.lexbuf in
              let toks = Lexing.lexeme st.lexbuf in
              let toks = if toks = "" then show_token tok else toks in
              let startp = st.lexbuf.lex_start_p and endp = st.lexbuf.lex_curr_p in
              let tok = (tok, startp, endp) in
              let last_tok = (toks, tok) in
              (tok, { st with incoming_toks = [ last_tok ]; generation_streak = 0 })
        in
        dbg (fun () -> say "READ %s\n" (show_token @@ pi1 token));
        let chkp = offer ckpt token in
        loop st chkp
    (* handling errors, two cases:
       1. the lookahead does not fit (fail to shift)
       2. the stack does not reduce *)
    | HandlingError env -> (
        dbg (fun () -> say "* ERROR: stack [%s]\n" (show_env env));
        match st.incoming_toks with
        (* 1.1 shift failure, the token is invalid (not even a token, just a piece of text) *)
        | (s, (t, b, e)) :: incoming_toks when is_error_token t ->
            dbg (fun () -> say "  LOOKAHEAD: %s (invalid token)\n" (show_token t));
            (* b and e are likely wrong after merge *)
            begin
              match two_err env with
              | Empty -> failwith "the grammar start symbol must include an atom for parse error"
              | ZeroErr (t0, env) ->
                  let t = merge_parse_error t0 t in
                  dbg (fun () -> say "  RECOVERY: push (squashed) %s on [%s]\n" (show_token t) (show_env env));
                  let chkp = offer (input_needed env) (valid t) in
                  let incoming_toks = (s, (t, b, e)) :: incoming_toks in
                  loop { st with incoming_toks } chkp
              | OneErr (t0, env, _, _) ->
                  (* TODO: factor with prev case *)
                  let t = merge_parse_error t0 t in
                  dbg (fun () -> say "  RECOVERY: push (squashed) %s on [%s]\n" (show_token t) (show_env env));
                  let chkp = offer (input_needed env) (valid t) in
                  let incoming_toks = (s, (t, b, e)) :: incoming_toks in
                  loop { st with incoming_toks } chkp
            end
        (* 1.1 shift failure, the token does not fit *)
        | next_token :: incoming_toks ->
            dbg (fun () -> say "  LOOKAHEAD: %s (out of place token)\n" (show_token (snd next_token |> pi1)));
            let acceptable_tokens, reducible_productions = automaton_possible_moves env (snd next_token |> pi2) in
            let productions = automaton_productions env in
            dbg (fun () -> say "    PROPOSE: reductions: %s\n" (show_prods reducible_productions));
            dbg (fun () -> say "    PROPOSE: tokens: %s\n" (show_gens acceptable_tokens));
            begin
              match
                (* TODO: do not receive incoming_tokens, the action forces them *)
                (* TODO: can incoming tokens be more than 1 token? maybe generate could return a list? *)
                handle_unexpected_token ~productions ~next_token ~more_tokens:incoming_toks ~reducible_productions
                  ~acceptable_tokens ~generation_streak:st.generation_streak
              with
              | TurnInto t, incoming_toks ->
                  dbg (fun () -> say "  RECOVERY: turn to %s and push\n" (show_token (snd t |> pi1)));
                  let chkp = offer (input_needed env) (snd t) in
                  loop { st with incoming_toks } chkp
              | Generate t, incoming_toks ->
                  dbg (fun () -> say "  RECOVERY: generate %s and push\n" (fst t));
                  let chkp = offer (input_needed env) (snd t) in
                  let errbuf = (snd t |> pi2, fst t) :: st.errbuf in
                  let generation_streak = st.generation_streak + 1 in
                  loop { st with incoming_toks; errbuf; generation_streak } chkp
              | Reduce p, incoming_toks ->
                  dbg (fun () -> say "  RECOVERY: reduce %s\n" (show_prod p));
                  let chkp = input_needed (force_reduction p env) in
                  loop { st with incoming_toks } chkp
            end
        (* 2. reduce failure, we fold the stack into an error *)
        | [] -> (
            dbg (fun () -> say "  STUCK\n");
            match two_err env with
            | Empty -> assert false
            | ZeroErr (t, env) ->
                (* 2.1 we turn the top of the stack into an error *)
                dbg (fun () -> say "  RECOVERY: push %s on [%s]\n" (show_token t) (show_env env));
                let chkp = offer (input_needed env) (valid t) in
                loop st chkp
            | OneErr (t0, _, t, env) ->
                (* 2.2 if the two top items are errors we merge them *)
                dbg (fun () -> say "  RECOVERY: push (squashed) %s on [%s]\n" (show_token t) (show_env env));
                let t = merge_parse_error t0 t in
                let chkp = offer (input_needed env) (valid t) in
                loop st chkp))

  let parse lexbuf =
    let chkp = main lexbuf.lex_curr_p in
    let st = { lexbuf; errbuf = []; generation_streak = 0; incoming_toks = [] } in
    loop st chkp
end
