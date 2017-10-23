%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int yylineno;
int yydebug = 1;
char* lastFunction = "";
extern void yyerror( char* );
extern int yylex();
%}

/*********************************************************
 ********************************************************/
%union {
  char* id;
}

%token <id> ID
%token INTVAL
%token DBLVAL
%token FLTVAL
%token STRVAL
%token CHARVAL

%token VOID
%token SHORT
%token LONG
%token DOUBLE
%token CHAR
%token INT
%token FLOAT

%token EQ
%token NE
%token GE
%token LE
%token GT
%token LT
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token OR
%token AND
%token BITOR
%token BITAND
%token BITXOR
%token NOT
%token COM
%token LSH
%token RSH
%token SET
%token SETADD
%token SETSUB
%token SETMUL
%token SETDIV
%token SETMOD
%token SETOR
%token SETAND
%token SETXOR
%token SETLSH
%token SETRSH

%token RETURN
%token DO
%token WHILE
%token FOR
%token SWITCH
%token CASE
%token DEFAULT
%token IF
%token ELSE
%token CONTINUE
%token BREAK
%token GOTO

%token UNSIGNED
%token TYPEDEF
%token STRUCT
%token UNION
%token CONST
%token STATIC
%token EXTERN
%token AUTO
%token REGISTER
%token SIZEOF

%start top

%%

/*********************************************************
 * The top op parsing rule, as set using the %start
 * directive above.
 ********************************************************/
top
  :
  | function top
  ;

function
  : func_signature '{' func_body '}'

func_signature
  : type ID '(' args ')' { printf("%s", $2); printf(";\n"); lastFunction = $2; }

func_body
  :
  | declaration func_body
  | statement func_body
  ;

func_call
  : ID '(' exprs ')' { printf("%s -> %s; \n", lastFunction, $1); }

declaration
  : type ID ';'
  | type MUL ID
  | type ID '[' INTVAL ']'
  ;

statement
  : ID SET expr ';'
  | ID '['INTVAL']' SET expr ';'
  | RETURN expr ';'
  | '{' statements '}'
  | func_call ';'
  | IF '(' expr ')' statement
  | IF '(' expr ')' statement ELSE statement
  | WHILE '(' expr ')' statement
  ;

args
  : /* empty rule */
  | param
  | param ',' args
  ;

param
  : type ID
  | type MUL ID
  | type ID '[' ']'
	| type MUL ID '[' ']'
  | type ID '['expr']'
	| type MUL ID '['expr']'
  ;

statements
  : statement
  | statement ',' statements
  ;

expr
  : INTVAL
  | FLTVAL
  | STRVAL
  | CHARVAL
  | expr op expr
  | func_call
  | ID
	| MUL ID
  | ID '[' INTVAL ']'
  ;

exprs
  :	/*    */
  | expr ',' exprs
  | expr
  ;

type
  : VOID
  | CHAR
  | SHORT
  | INT
  | LONG
  | FLOAT
  | DOUBLE
  ;

op
  :	EQ
  | NE
  | GT
  | LT
  | LE
  | GE
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | LSH
  | RSH
  | OR
  | AND
  | BITAND
  | BITOR
  | BITXOR
  | SET
  | SETADD
  | SETSUB
  | SETMUL
  | SETDIV
  | SETMOD
  | SETOR
  | SETAND
  | SETXOR
  | SETLSH
  | SETRSH
  ;

%%

/*********************************************************
 * This method is invoked by the parser whenever an
 * error is encountered during parsing; just print
 * the error to stderr.
 ********************************************************/
void yyerror( char *err ) {
    fprintf( stderr, "at line %d: %s\n", yylineno, err );
}

/*********************************************************
 * This is the main function for the function call
 * graph program. We invoke the parser and return the
 * error/success code generated by it.
 ********************************************************/
int main( int argc, const char *argv[] ) {
    printf( "digraph funcgraph {\n" );
    int res = yyparse();
    printf( "}\n" );
    return res;
}