open Mastic

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
  type t = Fun of string * Cmd.t list | Err of t Error.t [@@deriving show]

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
  type t = P of Func.t list | Err of t Error.t [@@deriving show]

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
