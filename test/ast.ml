open Mastic

module Expr = struct

  type t =
    | Lit of int
    | Mul of t * t
    | Add of t * t
    | Call of string * t ErrorList.t
    (* begin boilerplate *)
    | Err of t Error.t
  [@@deriving show]
  module Arg = struct let name = "Expr.t" type nonrec t = t let pp = pp end
  module List = ErrorList.Of(Arg)

  let (ErrorToken.Registered { of_token; match_token; build_token; is_err }) =
    ErrorToken.register "Expr.t"
      { show; match_ast_error = (function Err x -> Some x | _ -> None); build_ast_error = (fun x -> Err x) }
  (* end boilerplate *)
end

module Cmd = struct

  type t = Assign of string * Expr.t | If of Expr.t * t * t option | Err of t Error.t [@@deriving show]

  module Arg = struct let name = "Cmd.t" type nonrec t = t let pp = pp end
  module List = ErrorList.Of(Arg)

  let (ErrorToken.Registered { of_token; build_token; match_token; is_err }) =
    ErrorToken.register "Cmd.t"
      { show; match_ast_error = (function Err x -> Some x | _ -> None); build_ast_error = (fun x -> Err x) }
end

module Func = struct

  type t = Fun of string * Cmd.List.t | Err of t Error.t [@@deriving show]

  module Arg = struct let name = "Func.t" type nonrec t = t let pp = pp end
  module List = ErrorList.Of(Arg)

  let (ErrorToken.Registered { of_token; build_token; match_token; is_err }) =
    ErrorToken.register "Func.t"
      { show; match_ast_error = (function Err x -> Some x | _ -> None); build_ast_error = (fun x -> Err x) }
end

module Prog = struct
  type t = P of Func.List.t | Err of t Error.t [@@deriving show]

  let (ErrorToken.Registered { of_token; match_token; build_token; is_err }) =
    ErrorToken.register "Prog.t"
      { show; match_ast_error = (function Err x -> Some x | _ -> None); build_ast_error = (fun x -> Err x) }
end
