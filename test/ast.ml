open Mastic

module List = struct

type 'a t =
  | Nil
  | Cons of 'a * 'a t
  | Err of string * ('a t) Error.t
[@@deriving show]

  type ErrorToken.t += Err : string * 'a t -> ErrorToken.t

  let register : string -> (Format.formatter -> 'a -> unit) -> (_ -> 'a t) * ('a t -> ErrorToken.t) = fun name showf ->
    (* TODO, name unique *)
    let build_token = (fun x -> Err(name, Obj.magic x)) in
  let (ErrorToken.Registered { of_token }) =
    ErrorToken.register
      {
        show = show showf;
        match_token = (function Err(n,x) when n = name -> Some (Obj.magic x) | _ -> None);
        build_token;
        match_ast_error = (function Err(n,x) when n = name -> Some x | _ -> None);
        build_ast_error = (fun x -> Err(name,x));
      }
  in of_token, build_token

  let is_err = function Err _ -> true | _ -> false

end

module Expr = struct
  type t =
    | Lit of int
    | Mul of t * t
    | Add of t * t
    | Call of string * t list
    (* begin boilerplate *)
    | Err of t Error.t
  [@@deriving show]

  let is_err = function Err _ -> true | _ -> false

  type ErrorToken.t += Err of t

  let (ErrorToken.Registered { of_token }) =
    ErrorToken.register
      {
        show;
        match_token = (function Err x -> Some x | _ -> None);
        build_token = (fun x -> Err x);
        match_ast_error = (function Err x -> Some x | _ -> None);
        build_ast_error = (fun x -> Err x);
      }
  (* end boilerplate *)
end

module Cmd = struct
  
  type t = Assign of string * Expr.t | If of Expr.t * t * t option | Err of t Error.t [@@deriving show]

  let of_tokenL, mkErrL = List.register "Cmd.t" pp
  let is_err = function Err _ -> true | _ -> false

  type ErrorToken.t += Err of t

  let (ErrorToken.Registered { of_token }) =
    ErrorToken.register
      {
        show;
        match_token = (function Err x -> Some x | _ -> None);
        build_token = (fun x -> Err x);
        match_ast_error = (function Err x -> Some x | _ -> None);
        build_ast_error = (fun x -> Err x);
      }
end


module Func = struct

  type t = Fun of string * Cmd.t List.t | Err of t Error.t
  [@@deriving show]

  let of_tokenL, mkErrL = List.register "Fun.t" pp

  let is_err = function Err _ -> true | _ -> false

  type ErrorToken.t += Err of t

  let (ErrorToken.Registered { of_token }) =
    ErrorToken.register
      {
        show;
        match_token = (function Err x -> Some x | _ -> None);
        build_token = (fun x -> Err x);
        match_ast_error = (function Err x -> Some x | _ -> None);
        build_ast_error = (fun x -> Err x);
      }

end

module Prog = struct
  type t = P of Func.t List.t | Err of t Error.t [@@deriving show]

  let is_err = function Err _ -> true | _ -> false

  type ErrorToken.t += Err of t

  let (ErrorToken.Registered { of_token }) =
    ErrorToken.register
      {
        show;
        match_token = (function Err x -> Some x | _ -> None);
        build_token = (fun x -> Err x);
        match_ast_error = (function Err x -> Some x | _ -> None);
        build_ast_error = (fun x -> Err x);
      }
end
