
%token <int> INT
%token EOF

%start <Ast.expr> prog

%%

/* the rule for parsing a program */
prog:
  | e = expr; EOF { e } // I want the program to be a single expression and return the expression
  ;

expr:
  | i = INT {Int  i}
  ;