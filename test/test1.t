Fuzz 10

  $ echo 'fun f ( x := 1) fun g ( )' | ./calc.exe -fuzz 20 -seed 0
  input: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #1: fun f ( $ := 1) fun g ( )
  error:                   ^^^^^^^           recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [$;:=;1])])); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #2: fun f ; x := 1) fun g ( )
  error:                 ^^^^^^^^^^          recovered syntax error
  error:                 ^ completed with (
  error:                 ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [])); (Ast.Func.Err [;]); (Ast.Func.Err [x]);
       (Ast.Func.Err [:=]); (Ast.Func.Err [1]); (Ast.Func.Err [)]);
       (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #3: fu$ f ( x := 1) fun g ( )
  error:           ^^^^^^^^^^^^^^^^          recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fu]); (Ast.Func.Err [$]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)]); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #4: fun f ( x := 1)  un g ( )
  error:                            ^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Err [un]); (Ast.Func.Err [g]); (Ast.Func.Err [(]);
       (Ast.Func.Err [)])])
  
  fuzzed input #5: fun f ( x  = 1) fun g ( )
  error:                   ^^^^^^^           recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [x;=;1])])); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #6: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #7: fun f ( x :=;1) fun g ( )
  error:                       ^^^           recovered syntax error
  error:                       ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «[_]»)); (Ast.Cmd.Err [1])]
         ));
       (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #8: fu$ f ( x := 1) fun g ( )
  error:           ^^^^^^^^^^^^^^^^          recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fu]); (Ast.Func.Err [$]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)]); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #9: fu$ f ( x := 1) fun g ( )
  error:           ^^^^^^^^^^^^^^^^          recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fu]); (Ast.Func.Err [$]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)]); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #10: fun f ( x;:= 1) fun g ( )
  error:                     ^^^^^^           recovered syntax error
  error:                     ^ completed with :=
  error:                     ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", «[_]»)); (Ast.Cmd.Err [:=;1])]));
       (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #11: fun f ( x := 1)  un g ( )
  error:                             ^^^^^^^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Err [un]); (Ast.Func.Err [g]); (Ast.Func.Err [(]);
       (Ast.Func.Err [)])])
  
  fuzzed input #12: f n f ( x := 1) fun g ( )
  error:            ^^^^^^^^^^^^^^^^          recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [f]); (Ast.Func.Err [n]); (Ast.Func.Err [f]);
       (Ast.Func.Err [(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)]); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #13: fun f ( x := 1$ fun g ( )
  error:                         ^^^          recovered syntax error
  error:                            ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «[1;$]»))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #14: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #15: fun f ( x := 1) fun g ($)
  error:                                   ^^ recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", [(Ast.Cmd.Err [$])]))])
  note: not a subterm
  
  fuzzed input #16: fun f ( x := 1$ fun g ( )
  error:                         ^^^          recovered syntax error
  error:                            ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «[1;$]»))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #17: fun f ( $ := 1) fun g ( )
  error:                    ^^^^^^^           recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [$;:=;1])])); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #18: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #19: fun   ( x := 1) fun g ( )
  error:            ^^^^^^^^^^^^^^^^          recovered syntax error
  ast: (Ast.Prog.P
     [(Ast.Func.Err [fun;(]); (Ast.Func.Err [x]); (Ast.Func.Err [:=]);
       (Ast.Func.Err [1]); (Ast.Func.Err [)]); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #20: fun f ( x := 1) fun g   )
  error:                                    ^ completed with (
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", «1»))]));
       (Ast.Func.Fun ("g", []))])
  
