open Mastic

module Expr = struct
  type t =
    | Lit of int
    | Mul of t * t
    | Add of t * t
    | Call of string * t list
    | Err of t Error.t

  let is_err = function Err _ -> true | _ -> false

  let rec show = function
    | Lit n -> string_of_int n
    | Mul (a, b) -> Printf.sprintf "(%s*%s)" (show a) (show b)
    | Add (a, b) -> Printf.sprintf "(%s+%s)" (show a) (show b)
    | Err x -> Error.show show x
    | Call (s, l) ->
        Printf.sprintf "(%s %s)" s (String.concat " " (List.map show l))

  let show e = Printf.sprintf "«%s»" (show e)
  let pp fmt e = Format.fprintf fmt "%s" (show e)

  type ErrorToken.t += Expr of t

  let (ErrorToken.Registered { of_token }) =
    ErrorToken.register
      {
        show = (fun e -> "Expr[" ^ show e ^ "]");
        match_token = (function Expr x -> Some x | _ -> None);
        build_token = (fun x -> Expr x);
        match_ast_error = (function Err x -> Some x | _ -> None);
        build_ast_error = (fun x -> Err x);
      }
end

module Cmd = struct
  type t =
    | Assign of string * Expr.t
    | If of Expr.t * t * t option
    | Err of t Error.t
  [@@deriving show]

  let is_err = function Err _ -> true | _ -> false

  type ErrorToken.t += Cmd of t

  let (ErrorToken.Registered { of_token }) =
    ErrorToken.register
      {
        show = (fun e -> "Cmd[" ^ show e ^ "]");
        match_token = (function Cmd x -> Some x | _ -> None);
        build_token = (fun x -> Cmd x);
        match_ast_error = (function Err x -> Some x | _ -> None);
        build_ast_error = (fun x -> Err x);
      }
end

module Func = struct
  type t = Fun of string * Cmd.t list | Err of t Error.t [@@deriving show]

  let is_err = function Err _ -> true | _ -> false

  type ErrorToken.t += Func of t

  let (ErrorToken.Registered { of_token }) =
    ErrorToken.register
      {
        show = (fun e -> "Func[" ^ show e ^ "]");
        match_token = (function Func x -> Some x | _ -> None);
        build_token = (fun x -> Func x);
        match_ast_error = (function Err x -> Some x | _ -> None);
        build_ast_error = (fun x -> Err x);
      }
end

module Prog = struct
  type t = P of Func.t list | Err of t Error.t [@@deriving show]

  let is_err = function Err _ -> true | _ -> false

  type ErrorToken.t += Prog of t

  let (ErrorToken.Registered { of_token }) =
    ErrorToken.register
      {
        show = (fun e -> "Prog[" ^ show e ^ "]");
        match_token = (function Prog x -> Some x | _ -> None);
        build_token = (fun x -> Prog x);
        match_ast_error = (function Err x -> Some x | _ -> None);
        build_ast_error = (fun x -> Err x);
      }
end
