type 'a t = Nil | Cons of 'a * 'a t | Err of string * Error.t

let pp f fmt l =
  let rec pp1 fmt = function
    | Nil -> ()
    | Cons (x, Nil) -> f fmt x
    | Cons (x, xs) ->
        f fmt x;
        Format.fprintf fmt ";@ ";
        pp1 fmt xs
    | Err (_, e) -> Error.pp fmt e
  in
  Format.fprintf fmt "@[<hov 2>[%a]@]" pp1 l

let show f x = Format.asprintf "%a" (pp f) x

let rec included f e l1 l2 =
  match (l1, l2) with
  | Nil, Nil -> true
  | Cons (x, xs), Cons (y, ys) -> (f x y && included f e xs ys) || (e x && included f e xs l2)
  | Err _, _ -> true
  | _ -> false

let rec to_list acc = function Nil -> Some (List.rev acc) | Cons (x, xs) -> to_list (x :: acc) xs | Err _ -> None
let to_list x = to_list [] x
let rec has_err = function Nil -> false | Cons (_, xs) -> has_err xs | Err _ -> true

module type ListArg = sig
  val name : string

  type t

  val pp : Format.formatter -> t -> unit
end

module type ListSig = functor (X : ListArg) -> sig
  type nonrec t = X.t t
  type Error.t_ += List of t

  val name : string
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val of_token : Error.t -> t
  val build_token : t Error.located -> Error.t
  val iter : (X.t -> unit) -> (Error.t_ Error.located -> unit) -> t -> unit
end

module Of : ListSig =
functor
  (X : ListArg)
  ->
  struct
    type t_ = X.t t [@@deriving show]
    type t = t_ [@@deriving show]

    let name = X.name ^ " List.t"

    type Error.t_ += List of t

    let (Error.Registered { of_token; build_token }) =
      Error.register name
        {
          pp;
          match_ast = (function Err (n, x) when n = X.name -> Some x | _ -> None);
          build_ast = (fun x -> Err (X.name, x));
          match_error = (function List x -> Some x | _ -> None);
          build_error = (fun x -> List x);
        }

    let rec iter f g = function
      | Nil -> ()
      | Cons (x, xs) ->
          f x;
          iter f g xs
      | Err (n, e) ->
          assert (n = X.name);
          List.iter g e
  end
