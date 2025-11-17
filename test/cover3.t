Coverage

  $ echo 'fun x ( x :; else x ) ' | ./main.exe -debug
  random: 
  input: fun x ( x :; else x ) 
  READ Parser.FUN
  SHIFT [fun]
  READ (Parser.IDENT "x")
  SHIFT [fun; x]
  READ Parser.LPAREN
  SHIFT [fun; x; (]
  READ (Parser.IDENT "x")
  SHIFT [fun; x; (; x]
  READ (Parser.ERROR_TOKEN Lex[:])
  * ERROR: stack [fun; x; (; x]
    LOOKAHEAD: (Parser.ERROR_TOKEN Lex[:]) (invalid token)
    RECOVERY: push (squashed) (Parser.ERROR_TOKEN Lex[x;:]) on [fun; x; (]
  SHIFT [fun; x; (; perr]
  READ Parser.SEMICOLON
  RED 1 [fun; x; (; perr]
  SHIFT [fun; x; (; (Ast.Cmd.Err [x;:]); ;]
  READ Parser.ELSE
  * ERROR: stack [fun; x; (; (Ast.Cmd.Err [x;:]); ;]
    LOOKAHEAD: Parser.ELSE (out of place token)
      PROPOSE: reductions: 
      PROPOSE: tokens: 
    RECOVERY: generate _ and push (generation_streak = 0)
  SHIFT [fun; x; (; (Ast.Cmd.Err [x;:]); ;; perr]
  READ Parser.ELSE
  RED 1 [fun; x; (; (Ast.Cmd.Err [x;:]); ;; perr]
  * ERROR: stack [fun; x; (; (Ast.Cmd.Err [x;:]); ;; (Ast.Cmd.Err [_])]
    LOOKAHEAD: Parser.ELSE (out of place token)
      PROPOSE: reductions: [<list cmd> := <cmd>]
      PROPOSE: tokens: 
    RECOVERY: reduce [<list cmd> := <cmd>]
  READ Parser.ELSE
  RED 3 [fun; x; (; (Ast.Cmd.Err [x;:]); ;; (Ast.Cmd.Err [_])]
  * ERROR: stack [fun; x; (; (Ast.Cmd.Err [x;:]) (Ast.Cmd.Err [_])]
    LOOKAHEAD: Parser.ELSE (out of place token)
      PROPOSE: reductions: 
      PROPOSE: tokens: )
    RECOVERY: generate ) and push (generation_streak = 1)
  SHIFT [fun; x; (; (Ast.Cmd.Err [x;:]) (Ast.Cmd.Err [_]); )]
  READ Parser.ELSE
  RED 5 [fun; x; (; (Ast.Cmd.Err [x;:]) (Ast.Cmd.Err [_]); )]
  * ERROR: stack [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])]))]
    LOOKAHEAD: Parser.ELSE (out of place token)
      PROPOSE: reductions: 
      PROPOSE: tokens: 
    RECOVERY: turn to (Parser.ERROR_TOKEN Lex[else]) and push
  SHIFT [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); perr]
  READ (Parser.IDENT "x")
  RED 1 [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); perr]
  * ERROR: stack [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else])]
    LOOKAHEAD: (Parser.IDENT "x") (out of place token)
      PROPOSE: reductions: 
      PROPOSE: tokens: 
    RECOVERY: turn to (Parser.ERROR_TOKEN Lex[x]) and push
  SHIFT [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else]); perr]
  READ Parser.RPAREN
  RED 1 [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else]); perr]
  * ERROR: stack [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else]); (Ast.Func.Err [x])]
    LOOKAHEAD: Parser.RPAREN (out of place token)
      PROPOSE: reductions: 
      PROPOSE: tokens: 
    RECOVERY: turn to (Parser.ERROR_TOKEN Lex[)]) and push
  SHIFT [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else]); (Ast.Func.Err [x]); perr]
  READ Parser.EOF
  RED 1 [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else]); (Ast.Func.Err [x]); perr]
  RED 0 [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else]); (Ast.Func.Err [x]); (Ast.Func.Err [)])]
  RED 2 [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else]); (Ast.Func.Err [x]); (Ast.Func.Err [)]); ]
  RED 2 [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else]); (Ast.Func.Err [x]); (Ast.Func.Err [)])]
  RED 2 [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else]); (Ast.Func.Err [x]) (Ast.Func.Err [)])]
  RED 2 [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])); (Ast.Func.Err [else]) (Ast.Func.Err [x]) (Ast.Func.Err [)])]
  SHIFT [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])) (Ast.Func.Err [else]) (Ast.Func.Err [x]) (Ast.Func.Err [)]); eof]
  RED 2 [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])])) (Ast.Func.Err [else]) (Ast.Func.Err [x]) (Ast.Func.Err [)]); eof]
  ACCEPT
  error:         ^^^^ ^^^^^^^^^ recovered syntax error
  error:              ^ completed with _
  error:              ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("x", [(Ast.Cmd.Err [x;:]); (Ast.Cmd.Err [_])]));
       (Ast.Func.Err [else]); (Ast.Func.Err [x]); (Ast.Func.Err [)])])
  
