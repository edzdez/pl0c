%{
  module Mark = Util.Mark
  module Ident = Util.Ident
%}

%token <Util.Ident.t> IDENT
%token <Int32.t> NUMBER

%token CONST
%token VAR
%token PROCEDURE

%token WALRUS
%token CALL
%token QUESTION
%token BANG
%token BEGIN
%token END
%token IF
%token THEN
%token WHILE
%token DO

%token NOT
%token ODD

%token LPAREN
%token RPAREN
%token DOT
%token COMMA
%token SEMICOLON
%token EQ
%token NEQ
%token LT
%token LEQ
%token GT
%token GEQ
%token PLUS
%token MINUS
%token STAR
%token SLASH

%token EOF

%start <Ast.block option> program
%%

program:
  | EOF
    { None }
  | b = block; DOT; EOF
    { Some b }
  ;

m(x):
  | x = x;
    { let span = Mark.of_positions $startpos(x) $endpos(x) in
      Mark.create x span }
  ;

block:
  | const_decl = const_decl; var_decl = var_decl; proc_decl = proc_decl; stmt = stmt
    { { const_decl; var_decl; proc_decl; stmt }}
  ;

const_decl:
  | CONST; l = const_assignment_list; SEMICOLON
    { l }
  | { [] }
  ;

const_assignment_list:
  | x = m(IDENT); EQ; n = NUMBER
    { [(x, n)] }
  | x = m(IDENT); EQ; n = NUMBER; COMMA; xs = const_assignment_list
    { (x, n) :: xs }
  ;

var_decl:
  | VAR; l = ident_list; SEMICOLON
    { l }
  | { [] }
  ;

ident_list:
  | x = m(IDENT)
    { [x] }
  | x = m(IDENT); COMMA; xs = ident_list
    { x :: xs}
  ;

proc_decl:
  | PROCEDURE; p = m(IDENT); SEMICOLON; b = block; SEMICOLON; ps = proc_decl
    { (p, b) :: ps }
  | { [] }
  ;

stmt:
  | x = m(IDENT); WALRUS; e = m(expr)
    { Ast.Assign (x, e) }
  | CALL; p = m(IDENT)
    { Ast.Call p }
  | BEGIN; END
    { Ast.Scope [] }
  | BEGIN; l = stmt_list; END
    { Ast.Scope l }
  | IF; c = cond; THEN; s = stmt
    { Ast.If (c, s) }
  | WHILE; c = cond; DO; s = stmt
    { Ast.While (c, s) }
  | QUESTION; x = m(IDENT)
    { Ast.Read x }
  | BANG; e = m(factor)
    { Ast.Write e }
  ;

stmt_list:
  | s = stmt; SEMICOLON; ss = stmt_list
    { s :: ss }
  | s = stmt
    { [s] }
  ;

cond:
  | ODD; e = m(expr)
    { Ast.Odd e }
  | NOT; LPAREN; e = cond; RPAREN
    { Ast.Not e }
  | l = m(expr); op = rel; r = m(expr)
    { Rel (op, l, r) }

rel:
  | EQ
    { Ast.Eq }
  | NEQ
    { Ast.Neq }
  | LT
    { Ast.Lt }
  | LEQ
    { Ast.Leq }
  | GT
    { Ast.Gt }
  | GEQ
    { Ast.Geq }
  ;

expr:
  | t = term
    { t }
  | op = unary_op; t = m(term)
    { Ast.Unary (op, t) }
  | l = m(expr); op = add_op; r = m(term)
    { Ast.Binary (op, l, r) }
  ;

unary_op:
  | PLUS
    { Ast.Un_plus }
  | MINUS
    { Ast.Un_minus }
  ;

add_op:
  | PLUS
    { Ast.Plus }
  | MINUS
    { Ast.Minus }
  ;

term:
  | f = factor
    { f }
  | l = m(term); op = mult_op; r = m(factor)
    { Ast.Binary (op, l, r) }
  ;

mult_op:
  | STAR
    { Ast.Star }
  | SLASH
    { Ast.Slash }
  ;

factor:
  | x = m(IDENT)
    { Ast.Id x }
  | n = NUMBER
    { Ast.Num n }
  | LPAREN; e = expr; RPAREN
    { e }
  ;

