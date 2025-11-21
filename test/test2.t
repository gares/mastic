Fuzz 10

  $ echo 'fun f ( x := f x y )' | ./main.exe -fuzz 20 -rands 4,5,2,1,19,0,4,0,1,10,17,5,9,18,15,12,1,6,8,9
  random: 4,5,2,1,19,0,4,0,1,10,17,5,9,18,15,12,1,6,8,9
  input: fun f ( x := f x y )
  error:                    ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.Assign ("x",
               (Ast.Expr.Call ("f",
                  [
                    (Ast.Expr.Call ("x",
                       [(Ast.Expr.Call ("y", [(Ast.Expr.Err [('_',19,19)])]))]
                       ))]
                  ))
               ))]
          ))])
  
  fuzzed input #1: fun   ( x := f x y )
  error:                 ^ completed with _f
  error:                              ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("_f",
          Cmd.List.Err[
            (Ast.Cmd.Assign ("x",
               (Ast.Expr.Call ("f",
                  [
                    (Ast.Expr.Call ("x",
                       [(Ast.Expr.Call ("y", [(Ast.Expr.Err [('_',19,19)])]))]
                       ))]
                  ))
               ))]
          ))])
  note: not a subterm
  
  fuzzed input #2: fun f$( x := f x y )
  error:           ^^^ ^^^ ^ ^^ ^ ^ ^ ^ recovered syntax error
  error:           fun f$( x := f x y ) lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('fun',0,3); ('f',4,5); ('$',5,6); ('(',6,7); ('x',8,9);
            (':=',10,12); ('f',13,14); ('x',15,16); ('y',17,18); (')',19,20)])])
  
  fuzzed input #3: fu$ f ( x := f x y )
  error:           ^^^ ^ ^ ^ ^^ ^ ^ ^ ^ recovered syntax error
  error:           fu$ f ( x := f x y ) lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('fu',0,2); ('$',2,3); ('f',4,5); ('(',6,7); ('x',8,9);
            (':=',10,12); ('f',13,14); ('x',15,16); ('y',17,18); (')',19,20)])])
  
  fuzzed input #4: f n f ( x := f x y )
  error:           ^ ^ ^ ^ ^ ^^ ^ ^ ^ ^ recovered syntax error
  error:           f n f ( x := f x y ) lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('f',0,1); ('n',2,3); ('f',4,5); ('(',6,7); ('x',8,9); (':=',10,12);
            ('f',13,14); ('x',15,16); ('y',17,18); (')',19,20)])])
  
  fuzzed input #5: fun f ( x := f x y  
  error:                               ^ completed with _
  error:                               ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.Assign ("x",
               (Ast.Expr.Call ("f",
                  [
                    (Ast.Expr.Call ("x",
                       [(Ast.Expr.Call ("y", [(Ast.Expr.Err [('_',20,20)])]))]
                       ))]
                  ))
               ))]
          ))])
  
  fuzzed input #6: ;un f ( x := f x y )
  error:           ^^^ ^ ^ ^ ^^ ^ ^ ^ ^ recovered syntax error
  error:           ;un f ( x := f x y ) lex errors
  error:           ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('_',0,0); (';',0,1); ('un',1,3); ('f',4,5); ('(',6,7); ('x',8,9);
            (':=',10,12); ('f',13,14); ('x',15,16); ('y',17,18); (')',19,20)])])
  
  fuzzed input #7: fun   ( x := f x y )
  error:                 ^ completed with _f
  error:                              ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("_f",
          Cmd.List.Err[
            (Ast.Cmd.Assign ("x",
               (Ast.Expr.Call ("f",
                  [
                    (Ast.Expr.Call ("x",
                       [(Ast.Expr.Call ("y", [(Ast.Expr.Err [('_',19,19)])]))]
                       ))]
                  ))
               ))]
          ))])
  note: not a subterm
  
  fuzzed input #8: ;un f ( x := f x y )
  error:           ^^^ ^ ^ ^ ^^ ^ ^ ^ ^ recovered syntax error
  error:           ;un f ( x := f x y ) lex errors
  error:           ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('_',0,0); (';',0,1); ('un',1,3); ('f',4,5); ('(',6,7); ('x',8,9);
            (':=',10,12); ('f',13,14); ('x',15,16); ('y',17,18); (')',19,20)])])
  
  fuzzed input #9: f n f ( x := f x y )
  error:           ^ ^ ^ ^ ^ ^^ ^ ^ ^ ^ recovered syntax error
  error:           f n f ( x := f x y ) lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('f',0,1); ('n',2,3); ('f',4,5); ('(',6,7); ('x',8,9); (':=',10,12);
            ('f',13,14); ('x',15,16); ('y',17,18); (')',19,20)])])
  
  fuzzed input #10: fun f ( x  = f x y )
  error:                    ^  ^ ^ ^ ^   recovered syntax error
  error:                    x  = f x y   lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.Err
               [('x',8,9); ('=',11,12); ('f',13,14); ('x',15,16); ('y',17,18)])]
          ))])
  
  fuzzed input #11: fun f ( x := f x $ )
  error:                             ^   recovered syntax error
  error:                             $   lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.Assign ("x",
               (Ast.Expr.Call ("f",
                  [(Ast.Expr.Call ("x", [(Ast.Expr.Err [('$',17,18)])]))]))
               ))]
          ))])
  
  fuzzed input #12: fun f$( x := f x y )
  error:            ^^^ ^^^ ^ ^^ ^ ^ ^ ^ recovered syntax error
  error:            fun f$( x := f x y ) lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('fun',0,3); ('f',4,5); ('$',5,6); ('(',6,7); ('x',8,9);
            (':=',10,12); ('f',13,14); ('x',15,16); ('y',17,18); (')',19,20)])])
  
  fuzzed input #13: fun f ( x;:= f x y )
  error:                     ^ completed with :=
  error:                     ^ completed with _
  error:                      ^ completed with _x
  error:                               ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[(Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',9,9)])));
            (Ast.Cmd.Assign ("_x",
               (Ast.Expr.Call ("f",
                  [
                    (Ast.Expr.Call ("x",
                       [(Ast.Expr.Call ("y", [(Ast.Expr.Err [('_',19,19)])]))]
                       ))]
                  ))
               ))]
          ))])
  note: not a subterm
  
  fuzzed input #14: fun f ( x := f x y;)
  error:                              ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.Assign ("x",
               (Ast.Expr.Call ("f",
                  [
                    (Ast.Expr.Call ("x",
                       [(Ast.Expr.Call ("y", [(Ast.Expr.Err [('_',18,18)])]))]
                       ))]
                  ))
               ))]
          ))])
  
  fuzzed input #15: fun f ( x := f ; y )
  error:                           ^ completed with _
  error:                               ^ completed with :=
  error:                               ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.Assign ("x",
               (Ast.Expr.Call ("f", [(Ast.Expr.Err [('_',15,15)])]))));
            (Ast.Cmd.Assign ("y", (Ast.Expr.Err [('_',19,19)])))]
          ))])
  note: not a subterm
  
  fuzzed input #16: fun f ( x :=;f x y )
  error:                         ^ ^ ^   recovered syntax error
  error:                         f x y   lex errors
  error:                        ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[(Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',12,12)])));
            (Ast.Cmd.Err [('f',13,14); ('x',15,16); ('y',17,18)])]
          ))])
  note: not a subterm
  
  fuzzed input #17: f n f ( x := f x y )
  error:            ^ ^ ^ ^ ^ ^^ ^ ^ ^ ^ recovered syntax error
  error:            f n f ( x := f x y ) lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('f',0,1); ('n',2,3); ('f',4,5); ('(',6,7); ('x',8,9); (':=',10,12);
            ('f',13,14); ('x',15,16); ('y',17,18); (')',19,20)])])
  
  fuzzed input #18: fun f ; x := f x y )
  error:                  ^ ^ ^^ ^ ^ ^ ^ recovered syntax error
  error:                  ; x := f x y ) lex errors
  error:                  ^ completed with (
  error:                  ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[(Ast.Func.Fun ("f", Cmd.List.Err[]));
       (Ast.Func.Err
          [(';',6,7); ('x',8,9); (':=',10,12); ('f',13,14); ('x',15,16);
            ('y',17,18); (')',19,20)])])
  note: not a subterm
  
  fuzzed input #19: fun f ( $ := f x y )
  error:                    ^ ^^ ^ ^ ^ ^ recovered syntax error
  error:                    $ := f x y ) lex errors
  error:                      ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f", Cmd.List.Err[(Ast.Cmd.Err [('$',8,9)])]));
       (Ast.Func.Err
          [(':=',10,12); ('f',13,14); ('x',15,16); ('y',17,18); (')',19,20)])])
  note: not a subterm
  
  fuzzed input #20: fun f ( x;:= f x y )
  error:                     ^ completed with :=
  error:                     ^ completed with _
  error:                      ^ completed with _x
  error:                               ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[(Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',9,9)])));
            (Ast.Cmd.Assign ("_x",
               (Ast.Expr.Call ("f",
                  [
                    (Ast.Expr.Call ("x",
                       [(Ast.Expr.Call ("y", [(Ast.Expr.Err [('_',19,19)])]))]
                       ))]
                  ))
               ))]
          ))])
  note: not a subterm
  
