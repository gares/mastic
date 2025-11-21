Coverage

  $ echo 'fun $' | ./main.exe -debug
  random: 
  input: fun $
  READ Parser.FUN
  SHIFT [fun]
  READ (Parser.ERROR_TOKEN [('$',4,5)])
  * ERROR: stack [fun]
    LOOKAHEAD: (Parser.ERROR_TOKEN [('$',4,5)]) (invalid token)
    RECOVERY: push (squashed) (Parser.ERROR_TOKEN [('fun',0,3); ('$',4,5)]) on 
    []
  SHIFT [perr]
  READ Parser.EOF
  RED 1 [perr]
  RED 0 [(Ast.Func.Err [('fun',0,3); ('$',4,5)])]
  RED 2 [(Ast.Func.Err [('fun',0,3); ('$',4,5)]); Func.List.Err[]]
  SHIFT [Func.List.Err[(Ast.Func.Err [('fun',0,3); ('$',4,5)])]; eof]
  RED 2 [Func.List.Err[(Ast.Func.Err [('fun',0,3); ('$',4,5)])]; eof]
  ACCEPT
  error: ^^^ ^ recovered syntax error
  error: fun $ lex errors
  ast: (Ast.Prog.P Func.List.Err[(Ast.Func.Err [('fun',0,3); ('$',4,5)])])
  
