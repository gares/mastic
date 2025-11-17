Coverage

  $ echo 'x' | ./main.exe -debug
  random: 
  input: x
  READ (Parser.IDENT "x")
  * ERROR: stack []
    LOOKAHEAD: (Parser.IDENT "x") (out of place token)
      PROPOSE: reductions: 
      PROPOSE: tokens: 
    RECOVERY: turn (Parser.IDENT "x") into (Parser.ERROR_TOKEN [x]) and push
  SHIFT [perr]
  READ Parser.EOF
  RED 1 [perr]
  RED 0 [(Ast.Func.Err [x])]
  RED 2 [(Ast.Func.Err [x]); ]
  SHIFT [(Ast.Func.Err [x]); eof]
  RED 2 [(Ast.Func.Err [x]); eof]
  ACCEPT
  error: ^ recovered syntax error
  ast: (Ast.Prog.P [(Ast.Func.Err [x])])
  
