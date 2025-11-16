Fuzz 10

  $ echo 'fun f ( x := 1) fun g ( )' | ./main.exe -fuzz 20 -rands 19,10,7,16,14,0,4,20,21,20,17,10,24,3,15,12,1,6,3,19
  random: 19,10,7,16,14,0,4,20,21,20,17,10,24,3,15,12,1,6,3,19
  input: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #1: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #2: fun f ( x  = 1) fun g ( )
  error:                   ^^^^^^^           recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [x;=;1])])); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #3: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #4: fun f ( x := 1)  un g ( )
  error:                            ^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Err [un]); (Ast.Func.Err [g]); (Ast.Func.Err [(]);
       (Ast.Func.Err [)])])
  
  fuzzed input #5: fun f ( x := 1$ fun g ( )
  error:                        ^^^          recovered syntax error
  error:                           ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «[1;$]»))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #6: ;un f ( x := 1) fun g ( )
  error:           ^^^^^^^^^^^^^^^^          recovered syntax error
  error:           ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Err [_]); (Ast.Func.Err [;]); (Ast.Func.Err [un]);
       (Ast.Func.Err [f]); (Ast.Func.Err [(]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [1]); (Ast.Func.Err [)]);
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #7: fun   ( x := 1) fun g ( )
  error:           ^^^^^^^^^^^^^^^^          recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fun;(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)]); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #8: fun f ( x := 1) fun $ ( )
  error:                           ^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Err [fun;$]); (Ast.Func.Err [(]); (Ast.Func.Err [)])])
  
  fuzzed input #9: fun f ( x := 1) fun g;( )
  error:                                ^^^^ recovered syntax error
  error:                                ^ completed with (
  error:                                ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", [])); (Ast.Func.Err [;]); (Ast.Func.Err [(]);
       (Ast.Func.Err [)])])
  note: not a subterm
  
  fuzzed input #10: fun f ( x := 1) fun $ ( )
  error:                            ^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Err [fun;$]); (Ast.Func.Err [(]); (Ast.Func.Err [)])])
  
  fuzzed input #11: fun f ( x := 1) f$n g ( )
  error:                            ^^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Err [f]); (Ast.Func.Err [$]); (Ast.Func.Err [n]);
       (Ast.Func.Err [g]); (Ast.Func.Err [(]); (Ast.Func.Err [)])])
  
  fuzzed input #12: fun f ( x  = 1) fun g ( )
  error:                    ^^^^^^^           recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [x;=;1])])); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #13: fun f ( x := 1) fun g ( ;
  error:                                    ^ recovered syntax error
  error:                                    ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", [])); (Ast.Func.Err [;])])
  note: not a subterm
  
  fuzzed input #14: fun;f ( x := 1) fun g ( )
  error:               ^^^^^^^^^^^^^          recovered syntax error
  error:               ^ completed with ident
  error:               ^ completed with (
  error:               ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("_", [])); (Ast.Func.Err [;]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)]); (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #15: fun f ( x := 1);fun g ( )
  error:                           ^^         recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Err [;]); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #16: fun f ( x :=;1) fun g ( )
  error:                        ^^^           recovered syntax error
  error:                        ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «[_]»)); (Ast.Cmd.Err [1])]
         ));
       (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #17: f n f ( x := 1) fun g ( )
  error:            ^^^^^^^^^^^^^^^^          recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [f]); (Ast.Func.Err [n]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)]); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #18: fun f ; x := 1) fun g ( )
  error:                  ^^^^^^^^^^          recovered syntax error
  error:                  ^ completed with (
  error:                  ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [])); (Ast.Func.Err [;]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [1]); (Ast.Func.Err [)]);
       (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #19: fun;f ( x := 1) fun g ( )
  error:               ^^^^^^^^^^^^^          recovered syntax error
  error:               ^ completed with ident
  error:               ^ completed with (
  error:               ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("_", [])); (Ast.Func.Err [;]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)]); (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #20: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", []))])
  
