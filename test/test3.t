Fuxx

  $ echo 'fun f ( if 1 then x :=  else x := 1 )' | ./main.exe -fuzz 20 -rands 21,7,23,25,26,13,16,18,5,5,2,21,19,30,11,24,6,4,31,5
  random: 21,7,23,25,26,13,16,18,5,5,2,21,19,30,11,24,6,4,31,5
  input: fun f ( if 1 then x :=  else x := 1 )
  error:                         ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',24,24)]))),
               (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
          ))])
  
  fuzzed input #1: fun f ( if 1 then x :;  else x := 1 )
  error:                             ^ ^   ^^^^ ^ ^^ ^ ^ recovered syntax error
  error:                             x :   else x := 1 ) lex errors
  error:                                   ^ completed with _
  error:                                   ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('else',24,28); ('x',29,30); (':=',31,33); ('1',34,35); (')',36,37)]);
       (Ast.Func.Fun ("f",
          Cmd.List.Err[(Ast.Cmd.Err [('_',24,24)]);
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Err [('x',18,19); (':',20,21)]), None))]
          ))])
  note: not a subterm
  
  fuzzed input #2: fun f ( if 1 then x :=  else x := 1 )
  error:                                   ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',24,24)]))),
               (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
          ))])
  
  fuzzed input #3: fun f ( if 1 then x := $else x := 1 )
  error:                                  ^              recovered syntax error
  error:                                  $              lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Assign ("x", (Ast.Expr.Err [('$',23,24)]))),
               (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
          ))])
  
  fuzzed input #4: fun f ( if 1 then x :=  e se x := 1 )
  error:                                          ^^ ^ ^ recovered syntax error
  error:                                          := 1 ) lex errors
  error:                                          ^ completed with _
  error:                                          ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[(Ast.Func.Err [(':=',31,33); ('1',34,35); (')',36,37)]);
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Assign ("x",
                  (Ast.Expr.Call ("e",
                     [
                       (Ast.Expr.Call ("se",
                          [
                            (Ast.Expr.Call ("x", [(Ast.Expr.Err [('_',31,31)])]
                               ))]
                          ))]
                     ))
                  )),
               None))]
          ))])
  note: not a subterm
  
  fuzzed input #5: fun f ( if 1 then x :=  el$e x := 1 )
  error:                                     ^    ^^ ^ ^ recovered syntax error
  error:                                     $    := 1 ) lex errors
  error:                                          ^ completed with _
  error:                                          ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[(Ast.Func.Err [(':=',31,33); ('1',34,35); (')',36,37)]);
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Assign ("x",
                  (Ast.Expr.Call ("el",
                     [(Ast.Expr.Err [('$',26,27)]);
                       (Ast.Expr.Call ("e",
                          [
                            (Ast.Expr.Call ("x", [(Ast.Expr.Err [('_',31,31)])]
                               ))]
                          ))]
                     ))
                  )),
               None))]
          ))])
  note: not a subterm
  
  fuzzed input #6: fun f ( if 1  hen x :=  else x := 1 )
  error:                         ^^^ ^                   recovered syntax error
  error:                         hen x                   lex errors
  error:                               ^ completed with then
  error:                               ^ completed with _x
  error:                                   ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If (
               (Ast.Expr.Err
                  [((Ast.Expr.Lit 1),11,12); ('hen',14,17); ('x',18,19)]),
               (Ast.Cmd.Assign ("_x", (Ast.Expr.Err [('_',24,24)]))),
               (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
          ))])
  note: not a subterm
  
  fuzzed input #7: fun f ( if 1 the  x :=  else x := 1 )
  error:                        ^^^  ^                   recovered syntax error
  error:                        the  x                   lex errors
  error:                               ^ completed with then
  error:                               ^ completed with _x
  error:                                   ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If (
               (Ast.Expr.Err
                  [((Ast.Expr.Lit 1),11,12); ('the',13,16); ('x',18,19)]),
               (Ast.Cmd.Assign ("_x", (Ast.Expr.Err [('_',24,24)]))),
               (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
          ))])
  note: not a subterm
  
  fuzzed input #8: fun f ( if 1 then ; :=  else x := 1 )
  error:                                   ^^^^ ^ ^^ ^ ^ recovered syntax error
  error:                                   else x := 1 ) lex errors
  error:                             ^ completed with _
  error:                               ^ completed with _x
  error:                                   ^ completed with _
  error:                                   ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('else',24,28); ('x',29,30); (':=',31,33); ('1',34,35); (')',36,37)]);
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If ((Ast.Expr.Lit 1), (Ast.Cmd.Err [('_',18,18)]), None));
            (Ast.Cmd.Assign ("_x", (Ast.Expr.Err [('_',24,24)])))]
          ))])
  note: not a subterm
  
  fuzzed input #9: fun f$( if 1 then x :=  else x := 1 )
  error:           ^^^ ^^^ ^^ ^ ^^^^ ^ ^^  ^^^^ ^ ^^ ^ ^ recovered syntax error
  error:           fun f$( if 1 then x :=  else x := 1 ) lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('fun',0,3); ('f',4,5); ('$',5,6); ('(',6,7); ('if',8,10);
            ('1',11,12); ('then',13,17); ('x',18,19); (':=',20,22);
            ('else',24,28); ('x',29,30); (':=',31,33); ('1',34,35);
            (')',36,37)])])
  
  fuzzed input #10: fun f$( if 1 then x :=  else x := 1 )
  error:            ^^^ ^^^ ^^ ^ ^^^^ ^ ^^  ^^^^ ^ ^^ ^ ^ recovered syntax error
  error:            fun f$( if 1 then x :=  else x := 1 ) lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('fun',0,3); ('f',4,5); ('$',5,6); ('(',6,7); ('if',8,10);
            ('1',11,12); ('then',13,17); ('x',18,19); (':=',20,22);
            ('else',24,28); ('x',29,30); (':=',31,33); ('1',34,35);
            (')',36,37)])])
  
  fuzzed input #11: fu$ f ( if 1 then x :=  else x := 1 )
  error:            ^^^ ^ ^ ^^ ^ ^^^^ ^ ^^  ^^^^ ^ ^^ ^ ^ recovered syntax error
  error:            fu$ f ( if 1 then x :=  else x := 1 ) lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('fu',0,2); ('$',2,3); ('f',4,5); ('(',6,7); ('if',8,10);
            ('1',11,12); ('then',13,17); ('x',18,19); (':=',20,22);
            ('else',24,28); ('x',29,30); (':=',31,33); ('1',34,35);
            (')',36,37)])])
  
  fuzzed input #12: fun f ( if 1 then x :;  else x := 1 )
  error:                              ^ ^   ^^^^ ^ ^^ ^ ^ recovered syntax error
  error:                              x :   else x := 1 ) lex errors
  error:                                    ^ completed with _
  error:                                    ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('else',24,28); ('x',29,30); (':=',31,33); ('1',34,35); (')',36,37)]);
       (Ast.Func.Fun ("f",
          Cmd.List.Err[(Ast.Cmd.Err [('_',24,24)]);
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Err [('x',18,19); (':',20,21)]), None))]
          ))])
  note: not a subterm
  
  fuzzed input #13: fun f ( if 1 then x :=  else x := 1 )
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',24,24)]))),
               (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
          ))])
  
  fuzzed input #14: fun f ( if 1 then x :=  else x;:= 1 )
  error:                                    ^ completed with _
  error:                                          ^ completed with :=
  error:                                          ^ completed with _
  error:                                           ^ completed with _x
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',24,24)]))),
               (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',30,30)]))))));
            (Ast.Cmd.Assign ("_x", (Ast.Expr.Lit 1)))]
          ))])
  note: not a subterm
  
  fuzzed input #15: fun f ( if $ then x :=  else x := 1 )
  error:                       ^                          recovered syntax error
  error:                       $                          lex errors
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[
            (Ast.Cmd.If ((Ast.Expr.Err [('$',11,12)]),
               (Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',24,24)]))),
               (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
          ))])
  
  fuzzed input #16: fun f ( if 1 then x :=  ;lse x := 1 )
  error:                                     ^^^ ^ ^^ ^ ^ recovered syntax error
  error:                                     lse x := 1 ) lex errors
  error:                                    ^ completed with _
  error:                                           ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[(Ast.Func.Err [(':=',31,33); ('1',34,35); (')',36,37)]);
       (Ast.Func.Fun ("f",
          Cmd.List.Err[(Ast.Cmd.Err [('lse',25,28); ('x',29,30)]);
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',24,24)]))), None))]
          ))])
  note: not a subterm
  
  fuzzed input #17: fun f ; if 1 then x :=  else x := 1 )
  error:                  ^ ^^ ^ ^^^^ ^ ^^  ^^^^ ^ ^^ ^ ^ recovered syntax error
  error:                  ; if 1 then x :=  else x := 1 ) lex errors
  error:                  ^ completed with (
  error:                  ^ completed with )
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [(';',6,7); ('if',8,10); ('1',11,12); ('then',13,17); ('x',18,19);
            (':=',20,22); ('else',24,28); ('x',29,30); (':=',31,33);
            ('1',34,35); (')',36,37)]); (Ast.Func.Fun ("f", Cmd.List.Err[]))])
  note: not a subterm
  
  fuzzed input #18: fun   ( if 1 then x :=  else x := 1 )
  error:                  ^ completed with _f
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("_f",
          Cmd.List.Err[
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',24,24)]))),
               (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
          ))])
  note: not a subterm
  
  fuzzed input #19: fun f ( if 1 then x :=  else x  = 1 )
  error:                                         ^  ^ ^   recovered syntax error
  error:                                         x  = 1   lex errors
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Fun ("f",
          Cmd.List.Err[(Ast.Cmd.Err [('1',34,35)]);
            (Ast.Cmd.If ((Ast.Expr.Lit 1),
               (Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',24,24)]))),
               (Some (Ast.Cmd.Err [('x',29,30); ('=',32,33)]))))]
          ))])
  
  fuzzed input #20: fun f$( if 1 then x :=  else x := 1 )
  error:            ^^^ ^^^ ^^ ^ ^^^^ ^ ^^  ^^^^ ^ ^^ ^ ^ recovered syntax error
  error:            fun f$( if 1 then x :=  else x := 1 ) lex errors
  ast: (Ast.Prog.P
     Func.List.Err[
       (Ast.Func.Err
          [('fun',0,3); ('f',4,5); ('$',5,6); ('(',6,7); ('if',8,10);
            ('1',11,12); ('then',13,17); ('x',18,19); (':=',20,22);
            ('else',24,28); ('x',29,30); (':=',31,33); ('1',34,35);
            (')',36,37)])])
  

