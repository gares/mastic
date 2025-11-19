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
  module Arg = struct let name = "Expr.t" type nonrec t = t let pp = pp end
  module List = ErrorList.Of(Arg)

  type Error.t_ += Expr of t
  let (Error.Registered { of_token; build_token; is_err }) =
    Error.register "Expr.t"
      { pp;
        match_ast = (function Err x -> Some x | _ -> None);
        build_ast = (fun x -> Err x);
        match_error = (function Expr x -> Some x | _ -> None);
        build_error = (fun x -> Expr x) }
  (* end boilerplate *)
end

module Cmd = struct

  type t = Assign of string * Expr.t | If of Expr.t * t * t option | Err of Error.t [@@deriving show]

  module Arg = struct let name = "Cmd.t" type nonrec t = t let pp = pp end
  module List = ErrorList.Of(Arg)

  type Error.t_ += Cmd of t
  let (Error.Registered { of_token; build_token; is_err }) =
    Error.register "Cmd.t"
      { pp;
        match_ast = (function Err x -> Some x | _ -> None);
        build_ast = (fun x -> Err x);
        match_error = (function Cmd x -> Some x | _ -> None);
        build_error = (fun x -> Cmd x)
        }
end

module Func = struct

  type t = Fun of string * Cmd.List.t | Err of Error.t [@@deriving show]

  module Arg = struct let name = "Func.t" type nonrec t = t let pp = pp end
  module List = ErrorList.Of(Arg)

  type Error.t_ += Func of t
  let (Error.Registered { of_token; build_token; is_err }) =
    Error.register "Func.t"
      { pp;
      match_ast = (function Err x -> Some x | _ -> None);
      build_ast = (fun x -> Err x);
      match_error = (function Func x -> Some x | _ -> None);
      build_error = (fun x -> Func x) }
end

module Prog = struct
  type t = P of Func.List.t | Err of Error.t [@@deriving show]

  type Error.t_ += Prog of t
  let (Error.Registered { of_token; build_token; is_err }) =
    Error.register "Prog.t"
      { pp;
      match_ast = (function Err x -> Some x | _ -> None); 
      build_ast = (fun x -> Err x); 
      match_error = (function Prog x -> Some x | _ -> None);
      build_error = (fun x -> Prog x) }
end
