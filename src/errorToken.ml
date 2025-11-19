type t = Dyn of string * Obj.t

let inject_lex x = Error.map (fun x -> Error.Lex x) x
let inject_ast x = Error.map (fun x -> Error.Ast x) x

type 'a registration = {
  show : 'a -> string;
  match_ast_error : 'a -> 'a Error.t option;
  build_ast_error : 'a Error.t -> 'a;
}

type register = Token : string * 'a registration -> register

let registered = ref []

let lex_error = "LexError.t"
type lex_error_ty = string Error.located list
let mkLexError s b e =
  let v : lex_error_ty = [Error.loc s b e] in
  Error.loc (Dyn(lex_error,Obj.repr v)) b e

let show (Dyn(m,x)) =
  if m = lex_error then
    Obj.obj x
  else
  let rec aux = function
    | [] -> Printf.sprintf "<unregistered token %s>" m
    | Token(n,t) :: ps -> if n = m then t.show (Obj.obj x) else aux ps
  in
  aux !registered

let pp fmt e = Format.fprintf fmt "%s" (show e)

let of_token s ({ match_ast_error; build_ast_error } : 'a registration) : t Error.located -> 'a =
 fun x ->
  let b = Error.bloc x in
  let e = Error.eloc x in
  let Dyn(n,v) as x = Error.unloc x in
  if n = lex_error then
    let s : lex_error_ty = Obj.obj v in
     build_ast_error (List.map inject_lex s)
  else if n = s then 
    let a : 'a = Obj.obj v in
    match match_ast_error a with
    | Some _ -> a
    | None -> build_ast_error [ inject_ast (Error.loc a b e) ]
  else
    let s = show x in
    build_ast_error [inject_lex (Error.loc s b e)]

let match_token s (Dyn(n,o)) = if s = n then Some (Obj.obj o) else None
let build_token s o = Dyn(s,Obj.repr o)

let registration_of n =
  let rec aux = function
    | [] -> assert false
    | Token(s,_) as x :: _ when s = n -> x
    | _ :: ts -> aux ts
  in
    aux !registered

let merge (x : t Error.located) (y : t Error.located) : t Error.located =
  let Dyn(nx,vx) as x, bx, ex = Error.view x in
  let Dyn(ny,vy) as y, by, ey = Error.view y in
  (* if nx = lex_error then
    let vx : lex_error_ty = Obj.magic vx in
    let Token(_,t) = registration_of ny in
    let y = Option.get @@ t.match_ast_error (Option.get (match_token ny y)) in
    Error.loc (build_token ny @@ t.build_ast_error (Error.merge (List.map inject_lex vx) y)) bx ey
  else if ny = lex_error then
    let vy : lex_error_ty = Obj.magic vy in
    let Token(_,t) = registration_of nx in
    let x = Option.get @@ t.match_ast_error (Option.get (match_token nx x)) in
    Error.loc (build_token nx @@ t.build_ast_error (Error.merge (List.map inject_lex vy) x)) bx ey
  else *)
  let rec aux = function
    | [] -> assert false
    | Token(s,t) :: ps -> (
        let match_ast_error ba ea a =
          match t.match_ast_error a with
          | Some x -> x
          | None -> 
            [ inject_ast Error.(loc a ba ea) ]
        in
        match (match_token s x, match_token s y) with
        | Some x, Some y ->
            let e1 = match_ast_error bx ex x in
            let e2 = match_ast_error by ey y in
            build_token s @@ t.build_ast_error (Error.merge e1 e2)
        | None, None -> aux ps
        (* not sure this order makes sense *)
        | Some x, None ->
            let e1 = match_ast_error bx ex x in
            let e2 = match_ast_error by ey @@ of_token s t (Error.loc y by ey) in
            build_token s @@ t.build_ast_error (Error.merge e1 e2)
        | None, Some y ->
            let e1 = match_ast_error bx ex @@ of_token s t (Error.loc x bx ex) in
            let e2 = match_ast_error by ey y in
            build_token s @@ t.build_ast_error (Error.merge e1 e2))
  in
  Error.loc (aux !registered) bx ey

type 'a registered =
  Registered of {
    of_token : t Error.located -> 'a;
    build_token : 'a -> t;
    match_token : t -> 'a option;
    is_err : 'a -> bool
  }

let is_err { match_ast_error } x = match match_ast_error x with None -> false | _ -> true

let register : type a. string -> a registration -> a registered = fun s t ->
  registered := Token(s,t) :: !registered;
  Registered { of_token = of_token s t; match_token = match_token s; build_token = build_token s; is_err = is_err t }

let _ : lex_error_ty registered =
  let show l =
     Format.asprintf "Lex[%a]"
       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";") (Error.pp_located Format.pp_print_string)) l in
  register lex_error
    {
      show;
      match_ast_error = (fun l -> Some (List.map inject_lex l));
      build_ast_error = (fun l -> l |> List.map (Error.map (function Error.Lex s -> s | Error.Ast x -> show x)));
    }
