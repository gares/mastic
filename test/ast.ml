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

type 'a mylist =
  | Nil
  | Cons of 'a * 'a mylist
  | Err of ('a mylist) Error.t
[@@deriving show]
module Func = struct
  type t = Fun of string * cmd_list | Err of t Error.t
  and cmd_list =
  | Nil
  | Cons of Cmd.t * cmd_list
  | LErr of cmd_list Error.t
  [@@deriving show]

  let is_err = function Err _ -> true | _ -> false
  let is_lerr = function LErr _ -> true | _ -> false

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

  type ErrorToken.t += LErr of cmd_list

  let (ErrorToken.Registered { of_token = of_tokenL }) =
    ErrorToken.register
      {
        show = show_cmd_list;
        match_token = (function LErr x -> Some x | _ -> None);
        build_token = (fun x -> LErr x);
        match_ast_error = (function LErr x -> Some x | _ -> None);
        build_ast_error = (fun x -> LErr x);
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
