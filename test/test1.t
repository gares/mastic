Fuzz 10

  $ echo 'fun f ( x := 1) fun g ( )' | ./main.exe -fuzz 20 -rands 19,10,7,16,14,0,4,20,21,20,17,10,24,3,15,12,1,6,3,19
  random: 19,10,7,16,14,0,4,20,21,20,17,10,24,3,15,12,1,6,3,19
  input: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #1: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #2: fun f ( x  = 1) fun g ( )
  error:                   ^  ^ ^            recovered syntax error
  error:                   x  = 1            lex errors
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [('x',8,9); ('=',11,12); ('1',13,14)])]
         )); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #3: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #4: fun f ( x := 1)  un g ( )
  error:                            ^^ ^ ^ ^ recovered syntax error
  error:                            un g ( ) lex errors
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Err [('un',17,19)]); (Ast.Func.Err [('g',20,21)]);
       (Ast.Func.Err [('(',22,23)]); (Ast.Func.Err [(')',24,25)])])
  
  fuzzed input #5: fun f ( x := 1$ fun g ( )
  error:                         ^           recovered syntax error
  error:                         $           lex errors
  error:                           ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x",
             (Ast.Expr.Err [((Ast.Expr.Lit 1),13,14); ('$',14,15)])))]
         )); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #6: ;un f ( x := 1) fun g ( )
  error:           ^^^ ^ ^ ^ ^^ ^^           recovered syntax error
  error:           ;un f ( x := 1)           lex errors
  error:           ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Err [('_',0,0)]); (Ast.Func.Err [(';',0,1)]);
       (Ast.Func.Err [('un',1,3)]); (Ast.Func.Err [('f',4,5)]);
       (Ast.Func.Err [('(',6,7)]); (Ast.Func.Err [('x',8,9)]);
       (Ast.Func.Err [(':=',10,12)]); (Ast.Func.Err [('1',13,14)]);
       (Ast.Func.Err [(')',14,15)]); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #7: fun   ( x := 1) fun g ( )
  error:                 ^ completed with _f
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("_f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #8: fun f ( x := 1) fun $ ( )
  error:                           ^^^ ^ ^ ^ recovered syntax error
  error:                           fun $ ( ) lex errors
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Err [('fun',16,19); ('$',20,21)]);
       (Ast.Func.Err [('(',22,23)]); (Ast.Func.Err [(')',24,25)])])
  
  fuzzed input #9: fun f ( x := 1) fun g;( )
  error:                                ^^ ^ recovered syntax error
  error:                                ;( ) lex errors
  error:                                ^ completed with (
  error:                                ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Fun ("g", [])); (Ast.Func.Err [(';',21,22)]);
       (Ast.Func.Err [('(',22,23)]); (Ast.Func.Err [(')',24,25)])])
  note: not a subterm
  
  fuzzed input #10: fun f ( x := 1) fun $ ( )
  error:                            ^^^ ^ ^ ^ recovered syntax error
  error:                            fun $ ( ) lex errors
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Err [('fun',16,19); ('$',20,21)]);
       (Ast.Func.Err [('(',22,23)]); (Ast.Func.Err [(')',24,25)])])
  
  fuzzed input #11: fun f ( x := 1) f$n g ( )
  error:                            ^^^ ^ ^ ^ recovered syntax error
  error:                            f$n g ( ) lex errors
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Err [('f',16,17)]); (Ast.Func.Err [('$',17,18)]);
       (Ast.Func.Err [('n',18,19)]); (Ast.Func.Err [('g',20,21)]);
       (Ast.Func.Err [('(',22,23)]); (Ast.Func.Err [(')',24,25)])])
  
  fuzzed input #12: fun f ( x  = 1) fun g ( )
  error:                    ^  ^ ^            recovered syntax error
  error:                    x  = 1            lex errors
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Err [('x',8,9); ('=',11,12); ('1',13,14)])]
         )); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #13: fun f ( x := 1) fun g ( ;
  error:                                    ^ recovered syntax error
  error:                                    ; lex errors
  error:                                    ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Fun ("g", [])); (Ast.Func.Err [(';',24,25)])])
  note: not a subterm
  
  fuzzed input #14: fun;f ( x := 1) fun g ( )
  error:               ^^ ^ ^ ^^ ^^           recovered syntax error
  error:               ;f ( x := 1)           lex errors
  error:               ^ completed with _f
  error:               ^ completed with (
  error:               ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("_f", [])); (Ast.Func.Err [(';',3,4)]);
       (Ast.Func.Err [('f',4,5)]); (Ast.Func.Err [('(',6,7)]);
       (Ast.Func.Err [('x',8,9)]); (Ast.Func.Err [(':=',10,12)]);
       (Ast.Func.Err [('1',13,14)]); (Ast.Func.Err [(')',14,15)]);
       (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #15: fun f ( x := 1);fun g ( )
  error:                           ^          recovered syntax error
  error:                           ;          lex errors
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Err [(';',15,16)]); (Ast.Func.Fun ("g", []))])
  
  fuzzed input #16: fun f ( x :=;1) fun g ( )
  error:                         ^            recovered syntax error
  error:                         1            lex errors
  error:                        ^ completed with _
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f",
         [(Ast.Cmd.Assign ("x", (Ast.Expr.Err [('_',12,12)])));
           (Ast.Cmd.Err [('1',13,14)])]
         )); (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #17: f n f ( x := 1) fun g ( )
  error:            ^ ^ ^ ^ ^ ^^ ^^           recovered syntax error
  error:            f n f ( x := 1)           lex errors
  ast: (Ast.Prog.P
     [(Ast.Func.Err [('f',0,1)]); (Ast.Func.Err [('n',2,3)]);
       (Ast.Func.Err [('f',4,5)]); (Ast.Func.Err [('(',6,7)]);
       (Ast.Func.Err [('x',8,9)]); (Ast.Func.Err [(':=',10,12)]);
       (Ast.Func.Err [('1',13,14)]); (Ast.Func.Err [(')',14,15)]);
       (Ast.Func.Fun ("g", []))])
  
  fuzzed input #18: fun f ; x := 1) fun g ( )
  error:                  ^ ^ ^^ ^^           recovered syntax error
  error:                  ; x := 1)           lex errors
  error:                  ^ completed with (
  error:                  ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [])); (Ast.Func.Err [(';',6,7)]);
       (Ast.Func.Err [('x',8,9)]); (Ast.Func.Err [(':=',10,12)]);
       (Ast.Func.Err [('1',13,14)]); (Ast.Func.Err [(')',14,15)]);
       (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #19: fun;f ( x := 1) fun g ( )
  error:               ^^ ^ ^ ^^ ^^           recovered syntax error
  error:               ;f ( x := 1)           lex errors
  error:               ^ completed with _f
  error:               ^ completed with (
  error:               ^ completed with )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("_f", [])); (Ast.Func.Err [(';',3,4)]);
       (Ast.Func.Err [('f',4,5)]); (Ast.Func.Err [('(',6,7)]);
       (Ast.Func.Err [('x',8,9)]); (Ast.Func.Err [(':=',10,12)]);
       (Ast.Func.Err [('1',13,14)]); (Ast.Func.Err [(')',14,15)]);
       (Ast.Func.Fun ("g", []))])
  note: not a subterm
  
  fuzzed input #20: fun f ( x := 1) fun g ( )
  ast: (Ast.Prog.P
     [(Ast.Func.Fun ("f", [(Ast.Cmd.Assign ("x", (Ast.Expr.Lit 1)))]));
       (Ast.Func.Fun ("g", []))])
  
