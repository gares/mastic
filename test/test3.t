Fuxx

  $ echo 'fun f ( if 1 then x :=  else x := 1 )' | ./calc.exe -fuzz 20  -seed 0
  input: fun f ( if 1 then x :=  else x := 1 )
  error:                         ^             recovered syntax error
  error:                         ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «[_]»)),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #1: fun f ( if 1 then x $=  else x := 1 )
  error:                             ^^^^^               recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Err [x;$;=]),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #2: fun f ; if 1 then x :=  else x := 1 )
  error:                 ^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^ recovered syntax error
  error:                 ^ completed with (
  error:                 ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [])); (Ast.Func.Err [;]); (Ast.Func.Err [if]);
       (Ast.Func.Err [1]); (Ast.Func.Err [then]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [else]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  note: not a subterm
  
  fuzzed input #3: fun f ( if 1 then x :=  el$e x := 1 )
  error:                                     ^^   ^^^    recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1»,
             (Ast.Cmd.Assign ("x", «(el [$] (e (x [:=] 1)))»)), None))
           ]
         ))
       ])
  note: not a subterm
  
  fuzzed input #4: fun f ( if 1 the  x :=  else x := 1 )
  error:                      ^^^^^^^^^^^^ ^             recovered syntax error
  error:                                   ^ completed with then
  error:                                   ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («[1;the;x;:=]», (Ast.Cmd.Err [_]),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #5: fun f ( if 1 then x :=  else x := 1 )
  error:                                   ^             recovered syntax error
  error:                                   ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «[_]»)),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #6: fun f ( if 1 then x :=  else x := 1 )
  error:                                   ^             recovered syntax error
  error:                                   ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «[_]»)),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #7: fun f ( if 1 then x :=  ;lse x := 1 )
  error:                                   ^^^^^^^^^^^^  recovered syntax error
  error:                                   ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «[_]»)), None));
           (Ast.Cmd.Err [lse;x;:=;1])]
         ))
       ])
  note: not a subterm
  
  fuzzed input #8: fun f ( if 1 then x :=  el$e x := 1 )
  error:                                     ^^   ^^^    recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1»,
             (Ast.Cmd.Assign ("x", «(el [$] (e (x [:=] 1)))»)), None))
           ]
         ))
       ])
  note: not a subterm
  
  fuzzed input #9: fun f ( if 1 then x :=  el$e x := 1 )
  error:                                     ^^   ^^^    recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1»,
             (Ast.Cmd.Assign ("x", «(el [$] (e (x [:=] 1)))»)), None))
           ]
         ))
       ])
  note: not a subterm
  
  fuzzed input #10: fun f ( if 1 then x :;  else x := 1 )
  error:                              ^^^^  ^^^^^^^^^^^^^ recovered syntax error
  error:                                    ^ completed with _
  error:                                    ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Err [x;:]), None)); (Ast.Cmd.Err [_])]));
       (Ast.Func.Err [else]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)])])
  note: not a subterm
  
  fuzzed input #11: fun f ( if 1 then x :=  else x := 1 )
  error:                                    ^             recovered syntax error
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «[_]»)),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #12: fun f ( if 1 then x :=  e se x := 1 )
  error:                                           ^^^    recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «(e (se (x [:=] 1)))»)),
             None))
           ]
         ))
       ])
  note: not a subterm
  
  fuzzed input #13: fun f ( if 1 t$en x :=  else x := 1 )
  error:                       ^^^^^^^^^^^^ ^             recovered syntax error
  error:                                    ^ completed with then
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («[1;t;$;en;x;:=]», (Ast.Cmd.Err [_]),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #14: fun f ( if 1 then x :=  else x := 1 )
  error:                                    ^             recovered syntax error
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «[_]»)),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #15: fun f ( if 1 then x := $else x := 1 )
  error:                                   ^^             recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «[$]»)),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #16: fun f ( if 1 t$en x :=  else x := 1 )
  error:                       ^^^^^^^^^^^^ ^             recovered syntax error
  error:                                    ^ completed with then
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («[1;t;$;en;x;:=]», (Ast.Cmd.Err [_]),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #17: fun f ( if 1 then x :=  else x :$ 1 )
  error:                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^  recovered syntax error
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Err
             [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «[_]»)),
     (Some (Ast.Cmd.Err [x;:]))));$;1])
           ]
         ))
       ])
  
  fuzzed input #18: fun f ( if 1 then x :=  else x := 1 )
  error:                                    ^             recovered syntax error
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «[_]»)),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #19: fun f ( if 1 the  x :=  else x := 1 )
  error:                       ^^^^^^^^^^^^ ^             recovered syntax error
  error:                                    ^ completed with then
  error:                                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («[1;the;x;:=]», (Ast.Cmd.Err [_]),
             (Some (Ast.Cmd.Assign ("x", «1»)))))
           ]
         ))
       ])
  
  fuzzed input #20: fun f ( if 1 then x :=  else x :=   )
  error:                                    ^           ^ recovered syntax error
  error:                                    ^ completed with _
  error:                                                ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.If («1», (Ast.Cmd.Assign ("x", «[_]»)),
             (Some (Ast.Cmd.Assign ("x", «[_]»)))))
           ]
         ))
       ])
  

