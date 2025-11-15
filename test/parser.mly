%{ open Ast 


%}

%token <int> INT
%token PLUS TIMES
%token LPAREN RPAREN
%token EOF
%token FUN
%token IF
%token THEN
%token ELSE
%token ASSIGN
%token SEMICOLON
%token <string> IDENT
%token <Mastic.ErrorToken.t Mastic.Error.located> ERROR_TOKEN

%left PLUS        /* lowest precedence */
%left TIMES         /* medium precedence */

%start <Ast.Prog.t> main

%%

main:
| e = list(func); EOF { Prog.P e }
// | e = ERROR_TOKEN;  { Ast.Prog.of_token e }

func:
// | FUN; id = IDENT; LPAREN; bo = separated_list(SEMICOLON,cmd); RPAREN { Func.Fun(id,bo) }
| FUN; id = IDENT; LPAREN; bo = separated_list(SEMICOLON,cmd); RPAREN { Func.Fun(id,bo) }
| e = ERROR_TOKEN;  { Func.of_token e }

cmd:
| IF; e = expr; THEN; t = cmd { Cmd.If(e,t,None)}
| IF; e = expr; THEN; t = cmd; ELSE; t1 = cmd { Cmd.If(e,t,Some t1)}
| id = IDENT; ASSIGN; e = expr { Cmd.Assign(id,e) } 
| e = ERROR_TOKEN;  { Cmd.of_token e }

expr:
| i = INT { Expr.Lit i }
| LPAREN e = expr RPAREN { e }
| e1 = expr PLUS e2 = expr { Expr.Add (e1, e2) }
| e1 = expr TIMES e2 = expr { Expr.Mul(e1,e2) }
| f = IDENT; l = list(expr) { Expr.Call(f,l) } 
| e = ERROR_TOKEN;  { Expr.of_token e }
