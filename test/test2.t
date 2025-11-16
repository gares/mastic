Fuzz 10

  $ echo 'fun f ( x := f x y )' | ./main.exe -fuzz 20 -rands 4,5,2,1,19,0,4,0,1,10,17,5,9,18,15,12,1,6,8,9
  random: 4,5,2,1,19,0,4,0,1,10,17,5,9,18,15,12,1,6,8,9
  input: fun f ( x := f x y )
  error:                    ^ recovered syntax error
  error:                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «(f (x (y [_])))»))]))])
  
  fuzzed input #1: fun   ( x := f x y )
  error:           ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fun;(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [f]); (Ast.Func.Err [x]); (Ast.Func.Err [y]);
       (Ast.Func.Err [)])])
  
  fuzzed input #2: fun f$( x := f x y )
  error:           ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fun;f;$]); (Ast.Func.Err [(]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [f]); (Ast.Func.Err [x]);
       (Ast.Func.Err [y]); (Ast.Func.Err [)])])
  
  fuzzed input #3: fu$ f ( x := f x y )
  error:           ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fu]); (Ast.Func.Err [$]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [f]); (Ast.Func.Err [x]); (Ast.Func.Err [y]);
       (Ast.Func.Err [)])])
  
  fuzzed input #4: f n f ( x := f x y )
  error:           ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [f]); (Ast.Func.Err [n]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [f]); (Ast.Func.Err [x]); (Ast.Func.Err [y]);
       (Ast.Func.Err [)])])
  
  fuzzed input #5: fun f ( x := f x y  
  error:                               ^ completed with _
  error:                               ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «(f (x (y [_])))»))]))])
  
  fuzzed input #6: ;un f ( x := f x y )
  error:           ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  error:           ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Err [_]); (Ast.Func.Err [;]); (Ast.Func.Err [un]);
       (Ast.Func.Err [f]); (Ast.Func.Err [(]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [f]); (Ast.Func.Err [x]);
       (Ast.Func.Err [y]); (Ast.Func.Err [)])])
  
  fuzzed input #7: fun   ( x := f x y )
  error:           ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fun;(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [f]); (Ast.Func.Err [x]); (Ast.Func.Err [y]);
       (Ast.Func.Err [)])])
  
  fuzzed input #8: ;un f ( x := f x y )
  error:           ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  error:           ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Err [_]); (Ast.Func.Err [;]); (Ast.Func.Err [un]);
       (Ast.Func.Err [f]); (Ast.Func.Err [(]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [f]); (Ast.Func.Err [x]);
       (Ast.Func.Err [y]); (Ast.Func.Err [)])])
  
  fuzzed input #9: f n f ( x := f x y )
  error:           ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [f]); (Ast.Func.Err [n]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [f]); (Ast.Func.Err [x]); (Ast.Func.Err [y]);
       (Ast.Func.Err [)])])
  
  fuzzed input #10: fun f ( x  = f x y )
  error:                    ^^^^^^^^^^^  recovered syntax error
  ast: (Ast.Prog.P [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [x;=;f;x;y])]))])
  
  fuzzed input #11: fun f ( x := f x $ )
  error:                             ^^  recovered syntax error
  ast: (Ast.Prog.P [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «(f (x [$]))»))]))])
  
  fuzzed input #12: fun f$( x := f x y )
  error:            ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fun;f;$]); (Ast.Func.Err [(]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [f]); (Ast.Func.Err [x]);
       (Ast.Func.Err [y]); (Ast.Func.Err [)])])
  
  fuzzed input #13: fun f ( x;:= f x y )
  error:                     ^^^^^^^^^^  recovered syntax error
  error:                     ^ completed with :=
  error:                     ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «[_]»)); (Ast.Cmd.Err [:=;f;x;y])]))
       ])
  note: not a subterm
  
  fuzzed input #14: fun f ( x := f x y;)
  error:                              ^^ recovered syntax error
  error:                              ^ completed with _
  error:                               ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «(f (x (y [_])))»)); (Ast.Cmd.Err [_])]))
       ])
  note: not a subterm
  
  fuzzed input #15: fun f ( x := f ; y )
  error:                           ^   ^ recovered syntax error
  error:                           ^ completed with _
  error:                               ^ completed with :=
  error:                               ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «(f [_])»)); (Ast.Cmd.Assign ("y", «[_]»))]
         ))
       ])
  note: not a subterm
  
  fuzzed input #16: fun f ( x :=;f x y )
  error:                        ^^^^^^^  recovered syntax error
  error:                        ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «[_]»)); (Ast.Cmd.Err [f;x;y])]))
       ])
  note: not a subterm
  
  fuzzed input #17: f n f ( x := f x y )
  error:            ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [f]); (Ast.Func.Err [n]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [f]); (Ast.Func.Err [x]); (Ast.Func.Err [y]);
       (Ast.Func.Err [)])])
  
  fuzzed input #18: fun f ; x := f x y )
  error:                  ^^^^^^^^^^^^^^ recovered syntax error
  error:                  ^ completed with (
  error:                  ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [])); (Ast.Func.Err [;]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [f]); (Ast.Func.Err [x]);
       (Ast.Func.Err [y]); (Ast.Func.Err [)])])
  note: not a subterm
  
  fuzzed input #19: fun f ( $ := f x y )
  error:                    ^^^^^^^^^^^  recovered syntax error
  ast: (Ast.Prog.P [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [$;:=;f;x;y])]))])
  
  fuzzed input #20: fun f ( x;:= f x y )
  error:                     ^^^^^^^^^^  recovered syntax error
  error:                     ^ completed with :=
  error:                     ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «[_]»)); (Ast.Cmd.Err [:=;f;x;y])]))
       ])
  note: not a subterm
  
