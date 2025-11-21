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
  READ (Parser.ERROR_TOKEN [(':',10,11)])
  * ERROR: stack [fun; x; (; x]
    LOOKAHEAD: (Parser.ERROR_TOKEN [(':',10,11)]) (invalid token)
    RECOVERY: push (squashed) (Parser.ERROR_TOKEN [('x',8,9); (':',10,11)]) on 
    [fun; x; (]
  SHIFT [fun; x; (; perr]
  READ Parser.SEMICOLON
  RED 1 [fun; x; (; perr]
  SHIFT [fun; x; (; (Ast.Cmd.Err [('x',8,9); (':',10,11)]); ;]
  READ Parser.ELSE
  * ERROR: stack [fun; x; (; (Ast.Cmd.Err [('x',8,9); (':',10,11)]); ;]
    LOOKAHEAD: Parser.ELSE (out of place token)
      STATE: [<list cmd> <-- <cmd> ; <list cmd>]@2
      PROPOSE: reductions: 
      PROPOSE: tokens: 
    RECOVERY: generate hole and push (generation_streak = 0)
  SHIFT [fun; x; (; (Ast.Cmd.Err [('x',8,9); (':',10,11)]); ;; perr]
  READ Parser.ELSE
  RED 1 [fun; x; (; (Ast.Cmd.Err [('x',8,9); (':',10,11)]); ;; perr]
  * ERROR: stack [fun; x; (; (Ast.Cmd.Err [('x',8,9); (':',10,11)]); ;;
                   (Ast.Cmd.Err [('_',13,13)])]
    LOOKAHEAD: Parser.ELSE (out of place token)
      STATE: [<list cmd> <-- <cmd> ; <list cmd>]@1; [<list cmd> <-- <cmd>]@1
      PROPOSE: reductions: [<list cmd> <-- <cmd>]
      PROPOSE: tokens: 
    RECOVERY: reduce [<list cmd> <-- <cmd>]
  READ Parser.ELSE
  RED 3 [fun; x; (; (Ast.Cmd.Err [('x',8,9); (':',10,11)]); ;;
          Cmd.List.Err[(Ast.Cmd.Err [('_',13,13)])]]
  * ERROR: stack [fun; x; (;
                   Cmd.List.Err[
                     (Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]]
    LOOKAHEAD: Parser.ELSE (out of place token)
      STATE: [<func> <-- fun <ident> ( <list cmd> )]@4
      PROPOSE: reductions: 
      PROPOSE: tokens: )
    RECOVERY: generate ) and push (generation_streak = 1)
  SHIFT [fun; x; (;
          Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]; )]
  READ Parser.ELSE
  RED 5 [fun; x; (;
          Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]; )]
  * ERROR: stack [
                   (Ast.Func.Fun ("x",
                      Cmd.List.Err[
                        (Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
                      ))]
    LOOKAHEAD: Parser.ELSE (out of place token)
      STATE: [<list func> <-- <func> <list func>]@1
      PROPOSE: reductions: 
      PROPOSE: tokens: 
    RECOVERY: turn Parser.ELSE into (Parser.ERROR_TOKEN [('else',13,17)]) and push
  SHIFT [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             )); perr]
  READ (Parser.IDENT "x")
  RED 1 [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             )); perr]
  * ERROR: stack [
                   (Ast.Func.Fun ("x",
                      Cmd.List.Err[
                        (Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
                      )); (Ast.Func.Err [('else',13,17)])]
    LOOKAHEAD: (Parser.IDENT "x") (out of place token)
      STATE: [<list func> <-- <func> <list func>]@1
      PROPOSE: reductions: 
      PROPOSE: tokens: 
    RECOVERY: turn (Parser.IDENT "x") into (Parser.ERROR_TOKEN [('x',18,19)]) and push
  SHIFT [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             )); (Ast.Func.Err [('else',13,17)]); perr]
  READ Parser.RPAREN
  RED 1 [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             )); (Ast.Func.Err [('else',13,17)]); perr]
  * ERROR: stack [
                   (Ast.Func.Fun ("x",
                      Cmd.List.Err[
                        (Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
                      )); (Ast.Func.Err [('else',13,17)]);
                   (Ast.Func.Err [('x',18,19)])]
    LOOKAHEAD: Parser.RPAREN (out of place token)
      STATE: [<list func> <-- <func> <list func>]@1
      PROPOSE: reductions: 
      PROPOSE: tokens: 
    RECOVERY: turn Parser.RPAREN into (Parser.ERROR_TOKEN [(')',20,21)]) and push
  SHIFT [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             )); (Ast.Func.Err [('else',13,17)]); (Ast.Func.Err [('x',18,19)]);
          perr]
  READ Parser.EOF
  RED 1 [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             )); (Ast.Func.Err [('else',13,17)]); (Ast.Func.Err [('x',18,19)]);
          perr]
  RED 0 [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             )); (Ast.Func.Err [('else',13,17)]); (Ast.Func.Err [('x',18,19)]);
          (Ast.Func.Err [(')',20,21)])]
  RED 2 [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             )); (Ast.Func.Err [('else',13,17)]); (Ast.Func.Err [('x',18,19)]);
          (Ast.Func.Err [(')',20,21)]); Func.List.Err[]]
  RED 2 [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             )); (Ast.Func.Err [('else',13,17)]); (Ast.Func.Err [('x',18,19)]);
          Func.List.Err[(Ast.Func.Err [(')',20,21)])]]
  RED 2 [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             )); (Ast.Func.Err [('else',13,17)]);
          Func.List.Err[(Ast.Func.Err [('x',18,19); (')',20,21)])]]
  RED 2 [
          (Ast.Func.Fun ("x",
             Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
             ));
          Func.List.Err[
            (Ast.Func.Err [('else',13,17); ('x',18,19); (')',20,21)])]]
  SHIFT [
          Func.List.Err[
            (Ast.Func.Fun ("x",
               Cmd.List.Err[
                 (Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
               )); (Ast.Func.Err [('else',13,17); ('x',18,19); (')',20,21)])];
          eof]
  RED 2 [
          Func.List.Err[
            (Ast.Func.Fun ("x",
               Cmd.List.Err[
                 (Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]
               )); (Ast.Func.Err [('else',13,17); ('x',18,19); (')',20,21)])];
          eof]
  ACCEPT
  error:         ^ ^  ^^^^ ^ ^  recovered syntax error
  error:         x :  else x )  lex errors
  error:              ^ completed with _
  error:              ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("x",
          Cmd.List.Err[(Ast.Cmd.Err [('x',8,9); (':',10,11); ('_',13,13)])]));
       (Ast.Func.Err [('else',13,17); ('x',18,19); (')',20,21)])])
  
