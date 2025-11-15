
  open Lexing
  type 'a located = 'a * position * position
  let pp_located f fmt (s,_,_) = f fmt s
  let show_located f (s,_,_) = Format.asprintf "%a" f s

  type 'a t_ = Lex of string | Ast of 'a
  type 'a t = 'a t_ located list

  let loc x b e = x,b,e
  let unloc (x,_,_) = x
  let bloc (_,b,_) = b
  let eloc (_,_,e) = e
  let view x = x
  let map f (x,b,e) = (f x,b,e)

  let min_pos p1 p2 = if p1.pos_cnum < p2.pos_cnum then p1 else p2
  let max_pos p1 p2 = if p1.pos_cnum > p2.pos_cnum then p1 else p2

  let span l =
    let rec aux b e = function
      | [] -> b, e
      | x :: xs -> aux (min_pos b (bloc x)) (max_pos e (eloc x)) xs
    in
    match l with
    | [] -> assert false
    | x :: xs -> aux (bloc x) (eloc x) xs

  let merge = (@)

  let show show_a l =
    Printf.sprintf "[%s]"
      (String.concat ";" @@
        (List.map unloc l |> List.map (function Lex s -> s | Ast a -> show_a a)))
let pp f fmt e = 
  let f x = Format.asprintf "%a" f x in
  Format.fprintf fmt "%s" (show f e)