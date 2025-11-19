type 'a t = Nil | Cons of 'a * 'a t | Err of string * 'a t Error.t

let rec pp f fmt l =
  let rec pp1 fmt = function
    | Nil -> ()
    | Cons(x,Nil) -> f fmt x
    | Cons(x,xs) -> f fmt x; Format.fprintf fmt ";@ "; pp1 fmt xs
    | Err(_,e) -> Error.pp (pp f) fmt e in
  Format.fprintf fmt "@[<hov 2>[%a]@]" pp1 l

let show f x = Format.asprintf "%a" (pp f) x

let rec included f e l1 l2 =
  match (l1, l2) with
  | Nil, Nil -> true
  | Cons (x,xs), Cons(y,ys) -> (f x y && included f e xs ys) || (e x && included f e xs l2)
  | Err _, _ -> true
  | _ -> false

let rec to_list acc = function
  | Nil -> Some (List.rev acc)
  | Cons(x,xs) -> to_list (x::acc) xs
  | Err _ -> None
let to_list x = to_list [] x

let rec has_err = function Nil -> false | Cons(_,xs) -> has_err xs | Err _ -> true

module type ListArg = sig val name : string type t val pp : Format.formatter -> t -> unit end
module type ListSig = functor(X : ListArg) -> sig
  type nonrec t = X.t t
  val name : string
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val of_token : ErrorToken.t Error.located -> t
  val build_token : t -> ErrorToken.t
  val match_token : ErrorToken.t -> t option
  val is_err : t -> bool
  val iter : (X.t -> unit) -> (string Error.located -> unit) -> t -> unit
end

module Of : ListSig = functor(X : ListArg) -> struct
  type t_ = X.t t [@@deriving show]
  type t = t_ [@@deriving show]
  let name = X.name

    let (ErrorToken.Registered { of_token; build_token; match_token; is_err }) =
      ErrorToken.register (X.name ^ " List.t")
        {
          show = show;
          match_ast_error = (function Err (n, x) when n = X.name -> Some x | _ -> None);
          build_ast_error = (fun x -> Err (X.name, x));
        }


  let rec iter f g = function
    | Nil -> ()
    | Cons (x, xs) ->
        f x;
        iter f g xs
    | Err(n,e) -> assert(n = X.name);
        Error.iter (fun l -> iter f g (Error.unloc l)) g e
end

