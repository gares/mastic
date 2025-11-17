%{ open Ast 


%}

%token <int> INT
%token PLUS TIMES
%token LPAREN RPAREN
%token FUN
%token IF
%token THEN
%token ELSE
%token ASSIGN
%token SEMICOLON
%token <string> IDENT
/* boilerplate */ %token EOF
/* boilerplate */ %token <Mastic.ErrorToken.t Mastic.Error.located> ERROR_TOKEN

%left PLUS
%left TIMES

%start <Ast.Prog.t> main

%%

main:
| e = list_func; EOF { Prog.P e }
/* boilerplate */ | e = ERROR_TOKEN;  { Ast.Prog.of_token e }

func:
| FUN; id = IDENT; LPAREN; bo = list_cmd; RPAREN { Func.Fun(id,bo) }
/* boilerplate */ | e = ERROR_TOKEN;  { Func.of_token e }

list_func:
| { [] }
| f = func; l = list_func { f :: l }
/* boilerplate */ | e = ERROR_TOKEN; { [Func.of_token e] }

cmd:
| IF; e = expr; THEN; t = cmd { Cmd.If(e,t,None)}
| IF; e = expr; THEN; t = cmd; ELSE; t1 = cmd { Cmd.If(e,t,Some t1)}
| id = IDENT; ASSIGN; e = expr { Cmd.Assign(id,e) } 
/* boilerplate */ | e = ERROR_TOKEN;  { Cmd.of_token e }

list_cmd:
| { [] }
| c = cmd { [ c] }
| c = cmd; SEMICOLON; l = list_cmd { c :: l }
/* boilerplate */ | e = ERROR_TOKEN { [Cmd.of_token e] }

expr:
| i = INT { Expr.Lit i }
| LPAREN e = expr RPAREN { e }
| e1 = expr PLUS e2 = expr { Expr.Add (e1, e2) }
| e1 = expr TIMES e2 = expr { Expr.Mul(e1,e2) }
| f = IDENT; l = ne_list_expr { Expr.Call(f,l) } 
/* boilerplate */ | e = ERROR_TOKEN;  { Expr.of_token e }

ne_list_expr:
| e = expr; { [e] }
| e = expr; l = ne_list_expr { e :: l }
/* boilerplate */ | e = ERROR_TOKEN { [Expr.of_token e] }
