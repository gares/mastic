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
