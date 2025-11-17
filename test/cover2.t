Coverage

  $ echo 'fun $' | ./main.exe -debug
  random: 
  input: fun $
  error: ^^^^^ recovered syntax error
  ast: (Ast.Prog.P [(Ast.Func.Err [fun;$])])
  READ Parser.FUN
  SHIFT [fun]
  READ (Parser.ERROR_TOKEN Lex[$])
  * ERROR: stack [fun]
    LOOKAHEAD: (Parser.ERROR_TOKEN Lex[$]) (invalid token)
    RECOVERY: push (squashed) (Parser.ERROR_TOKEN Lex[fun;$]) on []
  SHIFT [perr]
  READ Parser.EOF
  RED 1 [perr]
  RED 0 [(Ast.Func.Err [fun;$])]
  RED 2 [(Ast.Func.Err [fun;$]); ]
  SHIFT [(Ast.Func.Err [fun;$]); eof]
  RED 2 [(Ast.Func.Err [fun;$]); eof]
  ACCEPT
  
