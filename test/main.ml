open Lexing

module ERParser =
  Mastic.ErrorResilientParser.Make
    (Parser.MenhirInterpreter)
    (Recovery.IncrementalParser)
    (Recovery.Strategy)

let underline l _ b e = Bytes.iteri (fun i _ -> if b.pos_cnum <= i && i < e.pos_cnum then Bytes.set l i '^' else ()) l

let write_lex_err l x b e =
  match x with
  | Mastic.Error.Lex s ->
      Bytes.iteri (fun i _ -> if b.pos_cnum <= i && i < e.pos_cnum then Bytes.set l i s.[i - b.pos_cnum] else ()) l
  | _ -> ()

let show_result header line errbuf v =
  let my_header = "error: " in
  let padding = String.length header - String.length my_header in
  let padding = String.make padding ' ' in

  let white = String.make (String.length line) ' ' in

  let b = String.to_bytes white in
  Ast.iter_prog (underline b) v;
  let b = String.of_bytes b in

  let c = String.to_bytes white in
  Ast.iter_prog (write_lex_err c) v;
  let c = String.of_bytes c in

  if b <> white then Printf.printf "%s%s%s recovered syntax error\n" my_header padding b;
  if c <> white then Printf.printf "%s%s%s lex errors\n" my_header padding c;

  List.rev errbuf
  |> List.iter (fun (p, s) ->
         let col = p.Lexing.pos_cnum in
         let msg = String.make col ' ' ^ "^ completed with " ^ s in
         Printf.printf "%s%s%s\n" my_header padding msg);
  Printf.printf "ast: %s\n" (Ast.Prog.show v);
  flush_all ()

let fuzz_with = [| ';'; ' '; '$' |]
let fuzz_with n = fuzz_with.(n mod Array.length fuzz_with)

let fuzz rands l =
  let m = List.hd !rands in
  rands := List.tl !rands;
  String.mapi (fun i c -> if i = m then fuzz_with m else c) l

let fuzz_no = ref 0
let only_fno = ref 0

let process rands (line : string) =
  let line = List.hd @@ String.split_on_char '\n' line in
  let header = "input: " in
  Printf.printf "%s%s\n" header line;
  let lexbuf = from_string line in
  let _, compbuf, v = ERParser.parse lexbuf in
  show_result header line compbuf v;
  Printf.printf "\n";
  for i = 1 to !fuzz_no do
    let line = fuzz rands line in
    if !only_fno < 1 || i = !only_fno then begin
      let header = Printf.sprintf "fuzzed input #%d: " i in
      Printf.printf "%s%s\n%!" header line;
      let lexbuf = from_string line in
      let _, compbuf', v' = ERParser.parse lexbuf in
      show_result header line compbuf' v';
      if not (Ast.included_prog v' v) then Printf.printf "note: not a subterm\n";
      Printf.printf "\n"
    end
  done

(* -------------------------------------------------------------------------- *)

(* The rest of the code is as in the [main] demo. *)

let rec draw_rands user_given how_many bound =
  if how_many = 0 then []
  else
    let r, user_given =
      match user_given with
      | [] -> (Random.int (bound - 1), user_given)
      | x :: user_given -> if x >= bound then exit 2 else (x, user_given)
    in
    r :: draw_rands user_given (how_many - 1) bound

let process rands line =
  let rands = draw_rands rands !fuzz_no (String.length line) in
  Printf.printf "random: %s\n" (String.concat "," (List.map string_of_int rands));
  process (ref rands) line

let main rands channel =
  let line = input_line channel in
  process rands line

let () =
  let file = ref stdin in
  let rands = ref "" in
  Arg.parse
    [
      ("-fuzz", Arg.Set_int fuzz_no, "how many fuzz (default 0)");
      ("-only", Arg.Set_int only_fno, "only run fuzz number N");
      ("-rands", Arg.Set_string rands, "random values (comma separated)");
      ("-debug", Arg.Set Mastic.ErrorResilientParser.debug, "verbose");
    ]
    (fun f -> file := open_in f)
    "help";
  let rands =
    String.split_on_char ',' !rands
    |> List.map (fun x -> try Some (int_of_string x) with _ -> None)
    |> List.filter_map (fun x -> x)
  in
  main rands !file
