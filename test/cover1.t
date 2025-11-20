Coverage

  $ echo 'x' | ./main.exe -debug
  random: 
  input: x
  READ (Parser.IDENT "x")
  * ERROR: stack []
    LOOKAHEAD: (Parser.IDENT "x") (out of place token)
      STATE: 
      PROPOSE: reductions: 
      PROPOSE: tokens: 
    RECOVERY: turn (Parser.IDENT "x") into (Parser.ERROR_TOKEN [('x',0,1)]) and push
  SHIFT [perr]
  READ Parser.EOF
  RED 1 [perr]
  RED 0 [(Ast.Func.Err [('x',0,1)])]
  RED 2 [(Ast.Func.Err [('x',0,1)]); []]
  SHIFT [[(Ast.Func.Err [('x',0,1)])]; eof]
  RED 2 [[(Ast.Func.Err [('x',0,1)])]; eof]
  ACCEPT
  error: ^ recovered syntax error
  error: x lex errors
  ast: (Ast.Prog.P [(Ast.Func.Err [('x',0,1)])])
  
