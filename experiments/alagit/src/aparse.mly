%{
  open Arith
%}

%token EOF
%token<int> INT
%token AND OR IMPL PLUS MINUS EQ NEQ LE LPAREN RPAREN ZERO SUCC

%left AND OR
%right IMPL
%nonassoc EQ NEQ LE
%left PLUS MINUS

%start<Arith.expr> expr
%%

expr: 
| a=e EOF {a}

e: 
| a=e AND b=e {And(a,b)}
| a=e OR b=e {Or(a,b)}
| a=e IMPL b=e {Impl(a,b)}
| a=e PLUS b=e {Plus(a,b)}
| a=e MINUS b=e {Minus(a,b)}
| a=e EQ b=e {Eq(a,b)}
| a=e NEQ b=e {Neq(a,b)}
| a=e LE b=e {Le(a,b)}
| LPAREN a=e RPAREN {a}
| ZERO  {O}
| SUCC a=e {S(a)}
| n=INT { let rec f n = if n<=0 then O else S(f(n-1)) in f n }
