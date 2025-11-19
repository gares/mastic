let debug = ref false

let dbg f =
  if !debug then (
    f ();
    flush_all ())
  else ()

let say = Printf.eprintf

type 'token tok = { s : string; t : 'token; b : Lexing.position; e : Lexing.position }

let tok_to_triple { t; b; e } = (t, b, e)

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

module Make
    (I : MenhirLib.IncrementalEngine.EVERYTHING)
    (M : IncrementalParser with type 'a checkpoint = 'a I.checkpoint and type token = I.token)
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
    | TopIsErr : token * 'x env -> 'x two_errors
    | Top2AreErr : token * token * 'x env -> 'x two_errors

  let ensure_top_is_error_token env =
    match top env with
    | None -> None
    | Some (Element (tx, x, b, e)) ->
        let tx = incoming_symbol tx in
        Some (is_error x tx, reduce_as_parse_error x tx b e, try_pop env)

  let ensure_top2_are_error_token env =
    match ensure_top_is_error_token env with
    | None -> Empty
    | Some (_, x, env1) -> (
          match ensure_top_is_error_token env1 with
          | None -> TopIsErr(x,env1)
          | Some (_, y, env2) -> Top2AreErr (x, y, env2))

  let tail = function [] -> [] | _ :: xs -> xs
  let rec drop n l = if n = 0 then l else drop (n - 1) (List.tl l)

  let automaton_productions env =
    match top env with
    | None -> []
    | Some (Element (st, _, _, _)) -> items st |> List.map (fun (p, i) -> (lhs p, rhs p, p, i))

  let valid t = match match_error_token t with None -> assert false | Some x -> (t, Error.bloc x, Error.eloc x)
  let is_error_token x = match match_error_token x with None -> false | _ -> true
  let show_element elt = match elt with Element (st, x, _, _) -> show_symbol (Some x) @@ incoming_symbol st

  let show_env env =
    let rec to_list env =
      match top env with None -> [] | Some x -> x :: (match pop env with None -> [] | Some x -> to_list x)
    in
    let stack = List.rev @@ to_list env in
    String.concat "; " (List.map show_element stack)

  let show_gens l = List.map (fun x -> x.s) l |> String.concat " "
  let show_xsymbol = function X s -> show_symbol None s

  let show_prod x =
    Printf.sprintf "[%s := %s]" (show_xsymbol (lhs x)) (String.concat " " (List.map show_xsymbol (rhs x)))

  let show_prods l = String.concat " " (List.map show_prod l)

  (* requires semantic actions to be pure *)
  let ensure_reduces : type a. a env -> production -> int -> production list =
   fun env prod pos ->
    try
      let _env : _ env = force_reduction prod env in
      [ prod ]
    with Invalid_argument _ -> []

  (* and next_symbols_opt = function None -> [] | Some env -> next_symbols env  *)
  let rec next_of (prod, pos) =
    match drop pos (rhs prod) with
    | [ X (T _); next ] when compare_symbols (lhs prod) next = 0 -> []
    | X (T x) :: _ -> token_of_terminal x |> o2l
    | X (N nt) :: _ when nullable nt -> next_of (prod, pos + 1)
    | _ -> []

  and o2l o = Option.fold ~none:[] ~some:(fun x -> [ x ]) o

  let automaton_possible_moves env b =
    match top env with
    | None -> ([], [])
    | Some (Element (st, _, _, _)) ->
        let items = items st in
        let reductions = List.concat_map (fun (p, i) -> ensure_reduces env p i) items in
        let tok (s, t) = { s; t; b; e = b } in
        let acceptable = List.concat_map next_of items |> List.map tok in
        (acceptable, reductions)

  type state = {
    lexbuf : lexbuf; (* the stream of tokens *)
    errbuf : (position * string) list; (* all tokens inserted *)
    incoming_toks : token tok list; (* the head of the token is the lookahead *)
    generation_streak : int; (* how many dummy tokens were generated since the last read from the stream *)
    ticks : int; (* when we reach eof we have at most ticks to terminate *)
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
        if st.ticks = 0 then
           invalid_arg "Mastic: too many loops. This should never happen, please report the issue"
        else
        let ((tok, _, _) as token), st =
          match st.incoming_toks with
          | t :: _ -> (tok_to_triple t, st)
          | [] ->
              let t = token st.lexbuf in
              let generation_streak, ticks =
                if is_eof_token t then st.generation_streak, st.ticks-1
                else 0, st.ticks+1 in
              let toks = Lexing.lexeme st.lexbuf in
              let s = if toks = "" then show_token t else toks in
              let b = st.lexbuf.lex_start_p and e = st.lexbuf.lex_curr_p in
              let last_tok = { s; t; b; e } in
              (tok_to_triple last_tok, { st with incoming_toks = [ last_tok ]; generation_streak; ticks })
        in
        dbg (fun () -> say "READ %s\n" (show_token @@ tok));
        let chkp = offer ckpt token in
        loop st chkp
    (* handling errors, two cases:
       1. the lookahead does not fit (fail to shift)
       2. the stack does not reduce *)
    | HandlingError env -> (
        dbg (fun () -> say "* ERROR: stack [%s]\n" (show_env env));
        match st.incoming_toks with
        (* 1.1 shift failure, the token is invalid (not even a token, just a piece of text) *)
        | { s; t; b; e } :: incoming_toks when is_error_token t ->
            dbg (fun () -> say "  LOOKAHEAD: %s (invalid token)\n" (show_token t));
            (* b and e are likely wrong after merge *)
            begin
              match ensure_top_is_error_token env with
              | None -> failwith "the grammar start symbol must include an atom for parse error"
              | Some (_, t0, env) ->
                  let t = merge_parse_error t0 t in
                  dbg (fun () -> say "  RECOVERY: push (squashed) %s on [%s]\n" (show_token t) (show_env env));
                  let chkp = offer (input_needed env) (valid t) in
                  let incoming_toks = { s; t; b; e } :: incoming_toks in
                  loop { st with incoming_toks } chkp
            end
        (* 1.1 shift failure, the token does not fit *)
        | next_token :: incoming_toks ->
            dbg (fun () -> say "  LOOKAHEAD: %s (out of place token)\n" (show_token next_token.t));
            let acceptable_tokens, reducible_productions = automaton_possible_moves env next_token.b in
            let productions = automaton_productions env in
            dbg (fun () -> say "    PROPOSE: reductions: %s\n" (show_prods reducible_productions));
            dbg (fun () -> say "    PROPOSE: tokens: %s\n" (show_gens acceptable_tokens));
            begin
              match
                (* TODO: do not receive incoming_tokens, the action forces them *)
                (* TODO: can incoming tokens be more than 1 token? maybe generate could return a list? *)
                handle_unexpected_token ~productions ~next_token ~reducible_productions
                  ~acceptable_tokens ~generation_streak:st.generation_streak
              with
              | TurnIntoError when is_eof_token next_token.t ->

                  begin match ensure_top2_are_error_token env with
                  | Empty
                  | TopIsErr _ -> assert false
                  | Top2AreErr (x,y,env) ->
                      let t = merge_parse_error x y in (* TODO: fix locs *)
                  let _,b,e as valid = valid t in
                  dbg (fun () ->
                      say "  RECOVERY: squash %s and %s and push\n" (show_token x) (show_token y));

                  let chkp = offer (input_needed env) valid in
                  let incoming_toks = { t ; s = ""; b; e } :: incoming_toks in
                  loop { st with incoming_toks } chkp

                  end
              | TurnIntoError ->
                  let t =
                    {
                      next_token with
                      t = build_error_token (ErrorToken.mkLexError next_token.s next_token.b next_token.e);
                    }
                  in
                  let incoming_toks = t :: incoming_toks in
                  dbg (fun () ->
                      say "  RECOVERY: turn %s into %s and push\n" (show_token next_token.t) (show_token t.t));
                  let chkp = offer (input_needed env) (tok_to_triple t) in
                  loop { st with incoming_toks } chkp
              | GenerateHole ->
                  let b = next_token.b in
                  let t =
                    {
                      s = "_";
                      t = build_error_token (ErrorToken.mkLexError "_" b b);
                      b = next_token.b;
                      e = next_token.b;
                    }
                  in
                  let incoming_toks = t :: next_token :: incoming_toks in
                  dbg (fun () ->
                      say "  RECOVERY: generate hole and push (generation_streak = %d)\n" st.generation_streak);
                  let chkp = offer (input_needed env) (tok_to_triple t) in
                  let errbuf = (t.b, t.s) :: st.errbuf in
                  let generation_streak = st.generation_streak + 1 in
                  loop { st with incoming_toks; errbuf; generation_streak } chkp
              | GenerateToken t ->
                  let incoming_toks = t :: next_token :: incoming_toks in
                  dbg (fun () ->
                      say "  RECOVERY: generate %s and push (generation_streak = %d)\n" t.s st.generation_streak);
                  let chkp = offer (input_needed env) (tok_to_triple t) in
                  let errbuf = (t.b, t.s) :: st.errbuf in
                  let generation_streak = st.generation_streak + 1 in
                  loop { st with incoming_toks; errbuf; generation_streak } chkp
              | Reduce p ->
                  let incoming_toks = next_token :: incoming_toks in
                  dbg (fun () -> say "  RECOVERY: reduce %s\n" (show_prod p));
                  let chkp = input_needed (force_reduction p env) in
                  loop { st with incoming_toks } chkp
            end
        (* 2. reduce failure, we fold the stack into an error *)
        | [] -> assert false)
  (* dbg (fun () -> say "  STUCK\n");
            match force_new_error_token env with
            | Empty -> assert false
            | TopIsErr (t, env) ->
                (* 2.1 we turn the top of the stack into an error *)
                dbg (fun () -> say "  RECOVERY: push %s on [%s]\n" (show_token t) (show_env env));
                let chkp = offer (input_needed env) (valid t) in
                loop st chkp
            | Top2AreErr (t0, t, env) ->
                (* 2.2 if the two top items are errors we merge them *)
                dbg (fun () -> say "  RECOVERY: push (squashed) %s on [%s]\n" (show_token t) (show_env env));
                let t = merge_parse_error t0 t in
                let chkp = offer (input_needed env) (valid t) in
                loop st chkp) *)

  let parse lexbuf =
    let chkp = main lexbuf.lex_curr_p in
    let st = { lexbuf; errbuf = []; generation_streak = 0; incoming_toks = []; ticks=1 } in
    loop st chkp
end
