Fuxx

  $ echo 'fun f ( if 1 then x :=  else x := 1 )' | ./main.exe -fuzz 20 -rands 21,7,23,25,26,13,16,18,5,5,2,21,19,30,11,24,6,4,31,5
  random: 21,7,23,25,26,13,16,18,5,5,2,21,19,30,11,24,6,4,31,5
  input: fun f ( if 1 then x :=  else x := 1 )
  error:                         ^             recovered syntax error
  error:                         ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1),
             (Ast.Cmd.Assign ("x", (Ast.Expr.Err [_]))),
             (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
         ))])
  
  fuzzed input #1: fun f ( if 1 then x :;  else x := 1 )
  error:                             ^^^^  ^^^^^^^^^^^^^ recovered syntax error
  error:                                   ^ completed with _
  error:                                   ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1), (Ast.Cmd.Err [ident;:]), None));
           (Ast.Cmd.Err [_])]
         )); (Ast.Func.Err [else]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  note: not a subterm
  
  fuzzed input #2: fun f ( if 1 then x :=  else x := 1 )
  error:                                   ^             recovered syntax error
  error:                                   ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1),
             (Ast.Cmd.Assign ("x", (Ast.Expr.Err [_]))),
             (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
         ))])
  
  fuzzed input #3: fun f ( if 1 then x := $else x := 1 )
  error:                                  ^^             recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1),
             (Ast.Cmd.Assign ("x", (Ast.Expr.Err [$]))),
             (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
         ))])
  
  fuzzed input #4: fun f ( if 1 then x :=  e se x := 1 )
  error:                                          ^^^    recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1),
             (Ast.Cmd.Assign ("x",
                (Ast.Expr.Call ("e",
                   [(Ast.Expr.Call ("se",
                       [(Ast.Expr.Call ("x",
                           [(Ast.Expr.Err [:=]); (Ast.Expr.Lit 1)]))]
                       ))]
                   ))
                )),
             None))]
         ))])
  note: not a subterm
  
  fuzzed input #5: fun f ( if 1 then x :=  el$e x := 1 )
  error:                                     ^^   ^^^    recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1),
             (Ast.Cmd.Assign ("x",
                (Ast.Expr.Call ("el",
                   [(Ast.Expr.Err [$]);
                     (Ast.Expr.Call ("e",
                        [(Ast.Expr.Call ("x",
                            [(Ast.Expr.Err [:=]); (Ast.Expr.Lit 1)]))]
                        ))]
                   ))
                )),
             None))]
         ))])
  note: not a subterm
  
  fuzzed input #6: fun f ( if 1  hen x :=  else x := 1 )
  error:                      ^^^^^^^^^^^^ ^             recovered syntax error
  error:                                   ^ completed with then
  error:                                   ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Err [(Ast.Expr.Lit 1);hen;x;:=]),
             (Ast.Cmd.Err [_]), (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))
             ))]
         ))])
  
  fuzzed input #7: fun f ( if 1 the  x :=  else x := 1 )
  error:                      ^^^^^^^^^^^^ ^             recovered syntax error
  error:                                   ^ completed with then
  error:                                   ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Err [(Ast.Expr.Lit 1);the;x;:=]),
             (Ast.Cmd.Err [_]), (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))
             ))]
         ))])
  
  fuzzed input #8: fun f ( if 1 then ; :=  else x := 1 )
  error:                             ^ ^^^ ^^^^^^^^^^^^^ recovered syntax error
  error:                             ^ completed with _
  error:                                   ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1), (Ast.Cmd.Err [_]), None));
           (Ast.Cmd.Err [:=])]
         )); (Ast.Func.Err [else]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  note: not a subterm
  
  fuzzed input #9: fun f$( if 1 then x :=  else x := 1 )
  error:           ^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fun;ident;$]); (Ast.Func.Err [(]); (Ast.Func.Err [if]);
       (Ast.Func.Err [1]); (Ast.Func.Err [then]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [else]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  
  fuzzed input #10: fun f$( if 1 then x :=  else x := 1 )
  error:            ^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fun;ident;$]); (Ast.Func.Err [(]); (Ast.Func.Err [if]);
       (Ast.Func.Err [1]); (Ast.Func.Err [then]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [else]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  
  fuzzed input #11: fu$ f ( if 1 then x :=  else x := 1 )
  error:            ^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fu]); (Ast.Func.Err [$]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [if]); (Ast.Func.Err [1]);
       (Ast.Func.Err [then]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [else]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  
  fuzzed input #12: fun f ( if 1 then x :;  else x := 1 )
  error:                              ^^^^  ^^^^^^^^^^^^^ recovered syntax error
  error:                                    ^ completed with _
  error:                                    ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1), (Ast.Cmd.Err [ident;:]), None));
           (Ast.Cmd.Err [_])]
         )); (Ast.Func.Err [else]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  note: not a subterm
  
  fuzzed input #13: fun f ( if 1 then x :=  else x := 1 )
  error:                                    ^             recovered syntax error
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1),
             (Ast.Cmd.Assign ("x", (Ast.Expr.Err [_]))),
             (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
         ))])
  
  fuzzed input #14: fun f ( if 1 then x :=  else x;:= 1 )
  error:                                    ^     ^^^^^^  recovered syntax error
  error:                                    ^ completed with _
  error:                                          ^ completed with :=
  error:                                          ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1),
             (Ast.Cmd.Assign ("x", (Ast.Expr.Err [_]))),
             (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Err [_]))))));
           (Ast.Cmd.Err [:=;1])]
         ))])
  note: not a subterm
  
  fuzzed input #15: fun f ( if $ then x :=  else x := 1 )
  error:                       ^^           ^             recovered syntax error
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Err [$]),
             (Ast.Cmd.Assign ("x", (Ast.Expr.Err [_]))),
             (Some (Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1))))))]
         ))])
  
  fuzzed input #16: fun f ( if 1 then x :=  ;lse x := 1 )
  error:                                    ^^^^^^^^^^^^  recovered syntax error
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If ((Ast.Expr.Lit 1),
             (Ast.Cmd.Assign ("x", (Ast.Expr.Err [_]))), None));
           (Ast.Cmd.Err [ident;x;:=;1])]
         ))])
  note: not a subterm
  
  fuzzed input #17: fun f ; if 1 then x :=  else x := 1 )
  error:                  ^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^ recovered syntax error
  error:                  ^ completed with (
  error:                  ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [])); (Ast.Func.Err [;]); (Ast.Func.Err [if]);
       (Ast.Func.Err [1]); (Ast.Func.Err [then]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [else]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  note: not a subterm
  
  fuzzed input #18: fun   ( if 1 then x :=  else x := 1 )
  error:            ^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fun;(]); (Ast.Func.Err [if]); (Ast.Func.Err [1]);
       (Ast.Func.Err [then]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [else]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  
  fuzzed input #19: fun f ( if 1 then x :=  else x  = 1 )
  error:                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^  recovered syntax error
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Err
             [(Ast.Cmd.If ((Ast.Expr.Lit 1), (Ast.Cmd.Assign ("x", (Ast.Expr.Err [_]))),
     (Some (Ast.Cmd.Err [ident;=]))));1])]
         ))])
  
  fuzzed input #20: fun f$( if 1 then x :=  else x := 1 )
  error:            ^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fun;ident;$]); (Ast.Func.Err [(]); (Ast.Func.Err [if]);
       (Ast.Func.Err [1]); (Ast.Func.Err [then]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [else]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  

