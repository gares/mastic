type 'a t = Nil | Cons of 'a * 'a t | Err of string * Error.t

let pp s f fmt l =
  let rec pp1 fmt = function
    | Nil -> ()
    | Cons (x, Nil) -> f fmt x
    | Cons (x, xs) ->
        f fmt x;
        Format.fprintf fmt ";@ ";
        pp1 fmt xs
    | Err (n, e) -> Format.fprintf fmt "%s:%a" n Error.pp e
  in
  Format.fprintf fmt "@[<hov 2>%s[@,%a]@]" s pp1 l

let rec included f e l1 l2 =
  match (l1, l2) with
  | Nil, Nil -> true
  | Cons (x, xs), Cons (y, ys) -> (f x y && included f e xs ys) || (e x && included f e xs l2)
  | Err _, _ -> true
  | _ -> false

let rec to_list acc = function Nil -> Some (List.rev acc) | Cons (x, xs) -> to_list (x :: acc) xs | Err _ -> None
let to_list x = to_list [] x
let rec has_err = function Nil -> false | Cons (_, xs) -> has_err xs | Err _ -> true
let rec of_list = function [] -> Nil | x :: xs -> Cons (x, of_list xs)

let concat_if_list (l : 'a t Error.located list) : 'a list option =
  if List.exists (fun x -> Error.unloc x |> has_err) l then None
  else Some (List.flatten @@ List.map Option.get (List.map to_list (List.map Error.unloc l)))

module type ListArg = sig
  val name : string

  type t

  val pp : Format.formatter -> t -> unit
  val registration : t Error.registration
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
  val mkCons : X.t -> t -> t
end

module Of : ListSig =
functor
  (X : ListArg)
  ->
  struct
    type t_ = X.t t

    let ppname = X.name ^ ".List.Err"
    let pp x = pp ppname X.pp x
    let name = X.name ^ ".List.t"
    let show x = Format.asprintf "%a" pp x

    type Error.t_ += List of t_

    let match_ast = function Err (n, x) when n = name -> Some x | _ -> None
    let match_error = function List x -> Some x | _ -> None

    let registration =
      { Error.pp; match_ast; build_ast = (fun x -> Err (name, x)); match_error; build_error = (fun x -> List x) }

    let (Error.Registered { of_token; build_token }) = Error.register name registration

    type t = t_

    let rec iter f g = function
      | Nil -> ()
      | Cons (x, xs) ->
          f x;
          iter f g xs
      | Err (n, e) ->
          assert (n = X.name);
          List.iter g e

    let mkCons (x : X.t) (xs : t) : t =
      let x = Cons (x, xs) in
      if has_err x then x
      else
        match to_list x with
        | None -> assert false
        | Some lx -> (
            let r = X.registration in
            let condition x = r.match_ast x in
            let err_X : Error.t = Error.squash X.registration @@ List.concat @@ List.filter_map condition lx in
            let other_X : X.t list = List.filter (fun x -> condition x = None) lx in
            let condition x = Error.omorph match_error x in
            let list_err_X = List.filter_map condition err_X in
            let err_other2 : Error.t = List.filter (fun x -> condition x = None) err_X in
            match concat_if_list @@ list_err_X with
            | None -> x
            | Some l ->
                let condition x = r.match_ast x in
                let err_X : Error.t = Error.squash X.registration @@ List.concat @@ List.filter_map r.match_ast l in
                let l : X.t list = List.filter (fun x -> condition x = None) l in
                let l = other_X @ l in
                if err_X = [] && err_other2 = [] then of_list l
                else of_list (l @ [ r.build_ast (Error.squash r (Error.merge err_X err_other2)) ]))
  end

let pp f l = pp "" f l
