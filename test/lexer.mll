{
  open Parser

}

rule token = parse
| [' ' '\t'] { token lexbuf }
| ['0'-'9']+ as i { INT (int_of_string i) }
| '+' { PLUS }
| '*' { TIMES }
| '(' { LPAREN }
| ')' { RPAREN }
| ';' { SEMICOLON }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "fun" { FUN }
| ":=" { ASSIGN }
| ['a'-'z']+ as id { IDENT id }
(* begin boilerplate *)
| eof { EOF }
| _ as x { ERROR_TOKEN (Mastic.ErrorToken.mkLexError (String.make 1 x) lexbuf.lex_start_p lexbuf.lex_curr_p) }
(* end boilerplate *)

