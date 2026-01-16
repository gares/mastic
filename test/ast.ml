open Mastic

module Expr = struct
  type t =
    | Lit of int
    | Mul of t * t
    | Add of t * t
    | Call of string * t ErrorList.t
    (* begin boilerplate *)
    | Err of Error.t
  [@@deriving show]

  type Error.t_ += Expr of t

  let registration =
    {
      Error.pp;
      match_ast = (function Err x -> Some x | _ -> None);
      build_ast = (fun x -> Err x);
      match_error = (function Expr x -> Some x | _ -> None);
      build_error = (fun x -> Expr x);
    }

  let (Error.Registered { of_token; build_token; is_err }) = Error.register "Expr.t" registration

  module Arg = struct
    let name = "Expr"

    type nonrec t = t

    let pp = pp
    let registration = registration
  end

  module List = ErrorList.Of (Arg)

  (* end boilerplate *)
end

module Cmd = struct
  type t = Assign of string * Expr.t | If of Expr.t * t * t option | Err of Error.t [@@deriving show]
  type Error.t_ += Cmd of t
  (* 

        let match_ast = (function Err x -> Some x | _ -> None)
        let build_ast = (fun x -> Err x)
        let match_error = (function Cmd x -> Some x | _ -> None)

   let squash x =
        let open Stdlib in
        let condition x = match_ast x in
        let same = List.concat @@ List.filter_map condition x in
        let other = List.filter (fun x -> condition x = None) x in
        build_ast (same @ other) *)

  let registration =
    {
      Error.pp;
      match_ast = (function Err x -> Some x | _ -> None);
      build_ast = (fun x -> Err x);
      match_error = (function Cmd x -> Some x | _ -> None);
      build_error = (fun x -> Cmd x);
    }

  let (Error.Registered { of_token; build_token; is_err }) = Error.register "Cmd.t" registration

  module Arg = struct
    let name = "Cmd"

    type nonrec t = t

    let pp = pp
    let registration = registration
  end

  module List = ErrorList.Of (Arg)
end

module Func = struct
  type t = Fun of string * Cmd.List.t | Err of Error.t [@@deriving show]
  type Error.t_ += Func of t

  let registration =
    {
      Error.pp;
      match_ast = (function Err x -> Some x | _ -> None);
      build_ast = (fun x -> Err x);
      match_error = (function Func x -> Some x | _ -> None);
      build_error = (fun x -> Func x);
    }

  let (Error.Registered { of_token; build_token; is_err }) = Error.register "Func.t" registration

  module Arg = struct
    let name = "Func"

    type nonrec t = t

    let pp = pp
    let registration = registration
  end

  module List = ErrorList.Of (Arg)
end

module Prog = struct
  type t = P of Func.List.t | Err of Error.t [@@deriving show]
  type Error.t_ += Prog of t

  let registration =
    {
      Error.pp;
      match_ast = (function Err x -> Some x | _ -> None);
      build_ast = (fun x -> Err x);
      match_error = (function Prog x -> Some x | _ -> None);
      build_error = (fun x -> Prog x);
    }

  let (Error.Registered { of_token; build_token; is_err }) = Error.register "Prog.t" registration
end

let included_opt f x y = match (x, y) with None, None -> true | Some x, Some y -> f x y | _ -> false

let rec included_prog : Prog.t -> Prog.t -> bool =
 fun x y ->
  let open Prog in
  match (x, y) with
  | Err _, _ -> true
  | P x, P y -> Mastic.ErrorList.included included_fun Func.is_err x y
  | _ -> false

and included_fun : Func.t -> Func.t -> bool =
 fun x y ->
  let open Func in
  match (x, y) with
  | Err _, _ -> true
  | Fun (n1, l1), Fun (n2, l2) -> n1 = n2 && Mastic.ErrorList.included included_cmd Cmd.is_err l1 l2
  | _ -> false

and included_cmd (x : Cmd.t) y =
  let open Cmd in
  match (x, y) with
  | Err _, _ -> true
  | Assign (n1, v1), Assign (n2, v2) -> n1 = n2 && included_expr v1 v2
  | If (v1, x1, oy1), If (v2, x2, oy2) -> included_expr v1 v2 && included_cmd x1 x2 && included_opt included_cmd oy1 oy2
  | _ -> false

and included_expr x y =
  let open Expr in
  match (x, y) with
  | Err _, _ -> true
  | Lit n, Lit m -> n = m
  | Mul (x1, y1), Mul (x2, y2) -> included_expr x1 y1 && included_expr x2 y2
  | Add (x1, y1), Add (x2, y2) -> included_expr x1 y1 && included_expr x2 y2
  | Call (f, xs), Call (g, ys) -> f = g && Mastic.ErrorList.included included_expr is_err xs ys
  | _ -> false

let rec iter_err b e f = function
  | Mastic.Error.Lex s as x -> f x b e
  | Expr.Expr e -> iter_expr f e
  | Expr.List.List e -> Expr.List.iter (iter_expr f) (iter_loc f) e
  | Cmd.Cmd e -> iter_cmd f e
  | Cmd.List.List e -> Cmd.List.iter (iter_cmd f) (iter_loc f) e
  | Prog.Prog e -> iter_prog f e
  | Func.Func e -> iter_func f e
  | Func.List.List e -> Func.List.iter (iter_func f) (iter_loc f) e
  | _ -> ()

and iter_loc f x =
  let b = Mastic.Error.bloc x in
  let e = Mastic.Error.eloc x in
  iter_err b e f (Mastic.Error.unloc x)

and iter_expr f = function
  | Expr.Add (e1, e2) ->
      iter_expr f e1;
      iter_expr f e2
  | Expr.Mul (e1, e2) ->
      iter_expr f e1;
      iter_expr f e2
  | Expr.Call (_, l) -> Expr.List.iter (iter_expr f) (iter_loc f) l
  | Expr.Lit _ -> ()
  | Err e -> List.iter (iter_loc f) e

and iter_func f = function
  | Func.Fun (_, l) -> Cmd.List.iter (iter_cmd f) (iter_loc f) l
  | Func.Err e -> List.iter (iter_loc f) e

and iter_cmd f = function
  | Cmd.Assign (_, v) -> iter_expr f v
  | Cmd.If (v, t, None) ->
      iter_expr f v;
      iter_cmd f t
  | Cmd.If (v, t, Some e) ->
      iter_expr f v;
      iter_cmd f t;
      iter_cmd f e
  | Err e -> List.iter (iter_loc f) e

and iter_prog f = function
  | Prog.P l -> Func.List.iter (iter_func f) (iter_loc f) l
  | Prog.Err e -> List.iter (iter_loc f) e
