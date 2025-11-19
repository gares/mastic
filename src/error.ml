open Lexing

type 'a located = 'a * position * position

let pp_located f fmt (s, b, e) = Format.fprintf fmt "('%a',%d,%d)" f s b.pos_cnum e.pos_cnum
let show_located f x = Format.asprintf "%a" (pp_located f) x

type 'a t_ = Lex of string | Ast of 'a
type 'a t = 'a t_ located list

let loc x b e = (x, b, e)
let unloc (x, _, _) = x
let bloc (_, b, _) = b
let eloc (_, _, e) = e
let view x = x
let map f (x, b, e) = (f x, b, e)
let iter f g l = List.iter (fun (x,b,e) -> match x with Lex s -> g (s,b,e) | Ast s -> f (s,b,e)) l
let min_pos p1 p2 = if p1.pos_cnum < p2.pos_cnum then p1 else p2
let max_pos p1 p2 = if p1.pos_cnum > p2.pos_cnum then p1 else p2

let span l =
  let rec aux b e = function [] -> (b, e) | x :: xs -> aux (min_pos b (bloc x)) (max_pos e (eloc x)) xs in
  match l with [] -> assert false | x :: xs -> aux (bloc x) (eloc x) xs

let merge = ( @ )

let pp f fmt e =
  let f fmt = function
    | Ast a -> f fmt a
    | Lex s -> Format.fprintf fmt "%s" s in
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";") (pp_located f)) e
let show f x = Format.asprintf "%a" (pp f) x
