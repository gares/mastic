Fuzz 10

  $ echo 'fun f ( x := f x y )' | ./main.exe -fuzz 20 -seed 0
  input: fun f ( x := f x y )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «(f (x (y )))»))]))])
  
  fuzzed input #1: f n f ( x := f x y )
  error:           ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [f]); (Ast.Func.Err [n]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [f]); (Ast.Func.Err [x]); (Ast.Func.Err [y]);
       (Ast.Func.Err [)])])
  
  fuzzed input #2: fun f ( x :=   x y )
  ast: (Ast.Prog.P [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «(x (y ))»))]))])
  note: not a subterm
  
  fuzzed input #3: fun f ( x;:= f x y )
  error:                    ^^^^^^^^^^  recovered syntax error
  error:                    ^ completed with :=
  error:                    ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «[_]»)); (Ast.Cmd.Err [:=;f;x;y])]))
       ])
  note: not a subterm
  
  fuzzed input #4: f n f ( x := f x y )
  error:           ^^^^^^^^^^^^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [f]); (Ast.Func.Err [n]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [f]); (Ast.Func.Err [x]); (Ast.Func.Err [y]);
       (Ast.Func.Err [)])])
  
  fuzzed input #5: fun f ( x := f x y )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «(f (x (y )))»))]))])
  
  fuzzed input #6: fun f ( $ := f x y )
  error:                   ^^^^^^^^^^^  recovered syntax error
  ast: (Ast.Prog.P [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [$;:=;f;x;y])]))])
  
  fuzzed input #7: fun f ( x := f$x y )
  error:                         ^^     recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «(f [$] (x (y )))»))]))])
  
  fuzzed input #8: fun f ; x := f x y )
  error:                 ^^^^^^^^^^^^^^ recovered syntax error
  error:                 ^ completed with (
  error:                 ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [])); (Ast.Func.Err [;]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [f]); (Ast.Func.Err [x]);
       (Ast.Func.Err [y]); (Ast.Func.Err [)])])
  note: not a subterm
  
  fuzzed input #9: fun f ( x :$ f x y )
  error:                   ^^^^^^^^^^^  recovered syntax error
  ast: (Ast.Prog.P [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [x;:;$;f;x;y])]))])
  
  fuzzed input #10: fun f ( x := f ; y )
  error:                               ^ recovered syntax error
  error:                               ^ completed with :=
  error:                               ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «(f )»)); (Ast.Cmd.Assign ("y", «[_]»))]))
       ])
  note: not a subterm
  
  fuzzed input #11: fun f ( x := f$x y )
  error:                          ^^     recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «(f [$] (x (y )))»))]))])
  
  fuzzed input #12: fun f ( x := f x y )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «(f (x (y )))»))]))])
  
  fuzzed input #13: fun f ; x := f x y )
  error:                  ^^^^^^^^^^^^^^ recovered syntax error
  error:                  ^ completed with (
  error:                  ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [])); (Ast.Func.Err [;]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [f]); (Ast.Func.Err [x]);
       (Ast.Func.Err [y]); (Ast.Func.Err [)])])
  note: not a subterm
  
  fuzzed input #14: fun f ( x := f x y;)
  error:                               ^ recovered syntax error
  error:                               ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «(f (x (y )))»)); (Ast.Cmd.Err [_])]))
       ])
  note: not a subterm
  
  fuzzed input #15: fun f ( x := f ; y )
  error:                               ^ recovered syntax error
  error:                               ^ completed with :=
  error:                               ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «(f )»)); (Ast.Cmd.Assign ("y", «[_]»))]))
       ])
  note: not a subterm
  
  fuzzed input #16: fun f ( $ := f x y )
  error:                    ^^^^^^^^^^^  recovered syntax error
  ast: (Ast.Prog.P [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [$;:=;f;x;y])]))])
  
  fuzzed input #17: fun f ( x;:= f x y )
  error:                     ^^^^^^^^^^  recovered syntax error
  error:                     ^ completed with :=
  error:                     ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «[_]»)); (Ast.Cmd.Err [:=;f;x;y])]))
       ])
  note: not a subterm
  
  fuzzed input #18: fun f ( x := f x y;)
  error:                               ^ recovered syntax error
  error:                               ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «(f (x (y )))»)); (Ast.Cmd.Err [_])]))
       ])
  note: not a subterm
  
  fuzzed input #19: fun f ( x :=;f x y )
  error:                        ^^^^^^^  recovered syntax error
  error:                        ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «[_]»)); (Ast.Cmd.Err [f;x;y])]))
       ])
  note: not a subterm
  
  fuzzed input #20: fun f ( x  = f x y )
  error:                    ^^^^^^^^^^^  recovered syntax error
  ast: (Ast.Prog.P [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [x;=;f;x;y])]))])
  
