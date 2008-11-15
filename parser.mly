%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS PLUSEQ MINUS MINUSEQ TIMES TIMESEQ DIVIDE DIVIDEEQ ASSIGN TRANSFER
%token EQ NEQ LT LEQ GT GEQ AND OR
%token ENTITYVAR GLOBALVAR ENTITYMEM
%token PRINT READ
%token RETURN IF ELSE FOR WHILE BREAK CONTINUE 
%token BOOL INT STRING CARD LIST VAR
%token CARDENTITIES GLOBALS INCLUDE PLAYORDER START WINNINGCONDITION
%token TRUE FALSE NULL
%token ME
%token <int> INTLITERAL
%token <string> STRINGLITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%left AND OR
%right ASSIGN PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ
%right READ
%right PRINT
%left TRANSFER
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left BOOL INT STRING CARD CARDENTITY LIST
%left ENTITYMEM
%left GLOBALVAR
%left ENTITYVAR

%start program
%type <Ast.program> program

%%

program:
  sdecl         { $1, [] }
| program fdecl { fst $1, ($2 :: snd $1) }

sdecl:
    INCLUDE LBRACE idecl_list RBRACE 
    CARDENTITIES LBRACE cdecl_list RBRACE
    GLOBALS LBRACE vdecl_list RBRACE 
    START LBRACE vdecl_list stmt_list RBRACE
    PLAYORDER LBRACE vdecl_list stmt_list RBRACE
    WINNINGCONDITION LBRACE vdecl_list stmt_list RBRACE
    { { incl = { includes = List.rev $3 };
	cent = { entities = List.rev $7 };
	glob = { globals = List.rev $11 };
	strt = { slocals = List.rev $15;
      		 sbody = List.rev $16 };
	play = { plocals = List.rev $20;
      		 pbody = List.rev $21 };
	wcon = { wlocals = List.rev $25;
      		 wbody = List.rev $26 } } }

fdecl:
    var LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { { fname = $1;
	formals = $3;
	locals = List.rev $6;
	body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

idecl_list:
    /* nothing */    { [] }
  | idecl_list idecl { $2 :: $1 }

idecl:
    ID SEMI { $1 }

cdecl_list:
    /* nothing */    { [] }
  | cdecl_list cdecl { $2 :: $1 }

cdecl:
    ID SEMI { $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    VAR INT ID SEMI { Var($3, None, Int) }
  | VAR STRING ID SEMI { Var($3, None, StringType) }
  | VAR BOOL ID SEMI { Var($3, None, Bool) }
  | CARD ID SEMI { Var($2, None, Card) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | expr PRINT { Print($1) }
  | expr READ { Read($1) }
  | BREAK SEMI { Break }
  | RETURN expr SEMI { Return($2) }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE /* %prec NOELSE */
      { If($3, $6, []) }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE
      { If($3, $6, $10) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN 
      LBRACE stmt_list RBRACE
      { For($3, $5, $7, $10) }
  | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3, $6) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    NULL             { Null }
  | TRUE             { BoolLiteral(True) }
  | FALSE            { BoolLiteral(False) }
  | INTLITERAL       { IntLiteral($1) }
  | STRINGLITERAL    { StringLiteral($1) }
  | var              { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr ASSIGN expr { Assign($1, $3) }
  | expr PLUSEQ expr { Assign($1, Binop($1, Add, $3)) }
  | expr MINUSEQ expr { Assign($1, Binop($1, Sub, $3)) }
  | expr TIMESEQ expr { Assign($1, Binop($1, Mult, $3)) }
  | expr DIVIDEEQ expr { Assign($1, Binop($1, Div, $3)) }
  | expr TRANSFER expr { Transfer($1, $3) }
  | var LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

var:
    ID                        { Var($1, None, Unknown) }
  | ENTITYVAR ID              { Var($2, None, CardEntity) }
  | ENTITYVAR ID ENTITYMEM ID { Var($4, Some($2), Unknown) }
  | GLOBALVAR ID              { Var($2, Global, Unknown) }
  | ME                        { Var("me", None, CardEntity) }
  | ME ENTITYMEM ID           { Var($3, Some("me"), Unknown) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
