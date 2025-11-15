
type t = ..

let inject_lex x = Error.map (fun x -> Error.Lex x) x
let inject_ast x = Error.map (fun x -> Error.Ast x) x



type 'a registration = {
  show : 'a -> string;
  match_token : t -> 'a option;
  build_token  : 'a -> t;
  match_ast_error : 'a -> 'a Error.t option;
  build_ast_error : 'a Error.t -> 'a;
}

type register = Token : 'a registration -> register
let registered = ref []


type t += LexError of string Error.located list
let mkLexError s b e = Error.loc (LexError[Error.loc s b e]) b e

let show x =
  let rec aux =
    function [] -> assert false
    | Token t :: ps ->
        match t.match_token x with
        | Some x -> t.show x
        | None -> aux ps
    in
      aux !registered

let pp fmt e = Format.fprintf fmt "%s" (show e)

let of_token ({ match_token; match_ast_error; build_ast_error; _ } : 'a registration) : t Error.located -> 'a = fun x ->
  let b = Error.bloc x in
  let e = Error.eloc x in
  let x = Error.unloc x in
  match match_token x with
  | None ->
      begin match x with
      | LexError l -> build_ast_error (List.map inject_lex l)
      | _ -> build_ast_error [inject_lex (Error.loc (show x) b e)]
      end
  | Some s ->
      match match_ast_error s with
      | Some _ -> s
      | None -> build_ast_error [inject_ast (Error.loc s b e)]

let merge (x : t Error.located) (y : t Error.located) : t Error.located =
  let x,bx,ex = Error.view x in
  let y,by,ey = Error.view y in
  let rec aux =
    function
    | [] -> assert false
    | Token t :: ps ->
        let match_ast_error ba ea a =
          match t.match_ast_error a with
          | Some x -> x
          | None -> [inject_ast Error.(loc a ba ea)] in
        match t.match_token x, t.match_token y with
        | Some x, Some y ->
            let e1 = match_ast_error bx ex x in
            let e2 = match_ast_error by ey y in
            t.build_token @@ t.build_ast_error (Error.merge e1 e2)
        | None, None -> aux ps
        (* not sure this order makes sense *)
        | Some x, None ->
              let e1 = match_ast_error bx ex x in
              let e2 = match_ast_error by ey @@ of_token t (Error.loc y by ey) in
              t.build_token @@ t.build_ast_error (Error.merge e1 e2)
        | None, Some y ->
              let e1 = match_ast_error bx ex @@ of_token t (Error.loc x bx ex) in
              let e2 = match_ast_error by ey y in
              t.build_token @@ t.build_ast_error (Error.merge e1 e2)
    in
      Error.loc (aux !registered) bx ey


type 'a registered = Registered of { of_token : t Error.located -> 'a }

let register (t : 'a registration) : 'a registered =
  registered := Token t :: !registered;
  Registered { of_token = of_token t }
;;

let _ : string Error.located list registered = 
  let show l = "Lex[" ^ String.concat ";" (List.map Error.unloc l) ^ "]" in
  register {
  show;
  match_token = (function LexError l -> Some l | _ -> None);
  build_token = (fun l -> LexError l);
  match_ast_error = (fun l -> Some (List.map inject_lex l));
  build_ast_error = (fun l -> l |>
    List.map (Error.map (function Error.Lex s -> s | Error.Ast x -> show x)));
}
