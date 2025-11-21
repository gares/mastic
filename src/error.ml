open Lexing

type 'a located = 'a * position * position

let pp_located f fmt (s, b, e) = Format.fprintf fmt "(%a,%d,%d)" f s b.pos_cnum e.pos_cnum
let show_located f x = Format.asprintf "%a" (pp_located f) x
let compare_located (_,b1,_) (_,b2,_) = Stdlib.compare b1.pos_cnum b2.pos_cnum
let loc x b e = (x, b, e)
let unloc (x, _, _) = x
let bloc (_, b, _) = b
let eloc (_, _, e) = e
let view x = x
let map f (x, b, e) = (f x, b, e)
let omorph f (x, b, e) =
  match f x with
  | None -> None
  | Some y -> Some (y, b, e)

type t_ = ..
type t = t_ located list

type 'a registration = {
  pp : Format.formatter -> 'a -> unit;
  match_ast : 'a -> t option;
  build_ast : t -> 'a;
  match_error : t_ -> 'a option;
  build_error : 'a -> t_;
}

type 'a registered = Registered of { of_token : t -> 'a; build_token : 'a located -> t; is_err : 'a -> bool }

module SM = Map.Make (String)

type xregistration = Registration : 'a registration -> xregistration

let registered = ref SM.empty

let pp fmt e =
  let f fmt x =
    let rec aux = function
      | [] -> assert false
      | (_, Registration r) :: rest -> (
          match r.match_error (unloc x) with
          | Some v -> pp_located r.pp fmt (loc v (bloc x) (eloc x))
          | None -> aux rest)
    in
    aux (SM.bindings !registered)
  in
  Format.fprintf fmt "@[<hov 2>[%a]@]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ") f) e

let show x = Format.asprintf "%a" pp x

let register name r : 'a registered =
  if SM.mem name !registered then failwith ("error " ^ name ^ " already registered");
  registered := SM.add name (Registration r) !registered;
  Registered
    {
      is_err = (fun x -> match r.match_ast x with Some _ -> true | None -> false);
      of_token = (fun x ->
        let condition (e,_,_) = Option.bind (r.match_error e) r.match_ast in
        let same = List.concat @@ List.filter_map condition x in
        let other = List.filter (fun x -> condition x = None) x in
        r.build_ast (same @ other));
      build_token = (fun x -> [ map r.build_error x ]);
    }

type t_ += Lex of string

let (Registered { build_token = mkLexError }) =
  register "__lex__"
    {
      pp = (fun fmt x -> Format.fprintf fmt "'%s'" x);
      match_ast = (fun _ -> None);
      build_ast = (fun _ -> assert false);
      match_error = (function Lex s -> Some s | _ -> None);
      build_error = (fun x -> Lex x);
    }

(* let iter f g l = List.iter (fun (x,b,e) -> match x with Lex s -> g (s,b,e) | Ast s -> f (s,b,e)) l *)
let min_pos p1 p2 = if p1.pos_cnum < p2.pos_cnum then p1 else p2
let max_pos p1 p2 = if p1.pos_cnum > p2.pos_cnum then p1 else p2

let span l =
  let rec aux b e = function [] -> (b, e) | x :: xs -> aux (min_pos b (bloc x)) (max_pos e (eloc x)) xs in
  match l with [] -> assert false | x :: xs -> aux (bloc x) (eloc x) xs

let merge x y = List.stable_sort compare_located (x @ y)

let rec squash (r : 'a registration) = function
  | [] -> []
  | x :: y :: xs ->
      begin match
        Option.bind (r.match_error (unloc x)) r.match_ast, 
        Option.bind (r.match_error (unloc y)) r.match_ast with
      | Some x, Some y ->  merge x y
      | _ -> x :: squash r (y :: xs)
      end
  | x :: xs -> x :: squash r xs 
