%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

//Define the tokens that you want to return from your lex as follows
#define PREPROC     256
#define ID          257
#define INTVAL      258
#define FLTVAL      259
#define DBLVAL      260
#define STRVAL      261
#define CHARVAL     262
#define VOID        263
#define CHAR        264
#define SHORT       265
#define INT         266
#define LONG        267
#define FLOAT       268
#define DOUBLE      269
#define EQ          270
#define NE          271
#define GE          272
#define LE          273
#define GT          274
#define LT          275
#define ADD         276
#define SUB         277
#define MUL         278
#define DIV         279
#define MOD         280
#define OR          281
#define AND         282
#define BITOR       283
#define BITAND      284
#define BITXOR      285
#define NOT         286
#define COM         287
#define LSH         288
#define RSH         289
#define SET         290
#define SETADD      291
#define SETSUB      292
#define SETMUL      293
#define SETDIV      294
#define SETMOD      295
#define SETOR       296
#define SETAND      297
#define SETXOR      298
#define SETLSH      299
#define SETRSH      300
#define RETURN      301
#define DO          302
#define WHILE       303
#define FOR         304
#define SWITCH      305
#define CASE        306
#define DEFAULT     307
#define IF          308
#define ELSE        309
#define CONTINUE    310
#define BREAK       311
#define GOTO        312
#define UNSIGNED    313
#define TYPEDEF     314
#define STRUCT      315
#define UNION       316
#define CONST       317
#define STATIC      318
#define EXTERN      319
#define AUTO        320
#define REGISTER    321
#define SIZEOF      322

int list_type;
%}

%option noyywrap nounput yylineno
%x SINGLECOMMENT MULTICOMMENT

PREPROC ^"#".*
ID [a-zA-Z][a-zA-Z0-9_]*
INTVAL [a-fA-F0-9]*
FLTVAL [0-9]*"."[0-9]*"f"
DBLVAL [0-9]*"."[0-9]*
STRVAL "\""[^\"]*"\""
CHARVAL "\'"(.|"\\".)"\'"
WHITESPACE [ \t\n]

%%
"//" {
  /******************************************
  * Matching with single line comments in our
  * C code
  ******************************************/
  BEGIN SINGLECOMMENT;
}

"/\*" {
  /******************************************
  * Matching with multiline comments in our
  * C code
  ******************************************/
  BEGIN MULTICOMMENT;
}

<SINGLECOMMENT>[\n](\r)? {
  /****************************************
  * A new line indicates end of a single
  * line comment in C
  ****************************************/
  BEGIN 0;
}

<MULTICOMMENT>"\*/" {
  /******************************************
  * Matching with end of multiline comments
  * in our C code
  ******************************************/
  BEGIN 0;
}

<SINGLECOMMENT,MULTICOMMENT>[ \t] {
  /****************************************
  * Lexer ignores the comments section
  * by Ignoring anything inside a comment
  ****************************************/
}

<SINGLECOMMENT,MULTICOMMENT>. {
}

{PREPROC} {
  /**************************************
  * If a preprocessor "#" is matched,
  * then print PREPROC and return a
  * token PREPROC
  **************************************/
  printf("PREPROC\n"); return PREPROC;
}

"(" { printf("(\n"); return '('; }
")" { printf(")\n"); return ')'; }
"{" { printf("{\n"); return '{'; }
"}" { printf("}\n"); return '}'; }
"[" { printf("[\n"); return '['; }
"]" { printf("]\n"); return ']'; }
"," { printf(",\n"); return ','; }
";" { printf(";\n"); return ';'; }

"void" { printf("VOID\n"); return VOID; }
"char" { printf("CHAR\n"); return CHAR; }
"short" { printf("SHORT\n"); return SHORT; }
"int" { printf("INT\n"); return INT; }
"long" { printf("LONG\n"); return LONG; }
"float" { printf("FLOAT\n"); return FLOAT; }
"double" { printf("DOUBLE\n"); return DOUBLE; }

"==" { printf("EQ\n"); return EQ; }
"!=" { printf("NE\n"); return NE; }
">=" { printf("GE\n"); return GE; }
"<=" { printf("LE\n"); return LE; }
">" { printf("GT\n"); return GT; }
"<" { printf("LT\n"); return LT; }
"+" { printf("ADD\n"); return ADD; }
"-" { printf("SUB\n"); return SUB; }
"*" { printf("MUL\n"); return MUL; }
"/" { printf("DIV\n"); return DIV; }
"%" { printf("MOD\n"); return MOD; }
"||" { printf("OR\n"); return OR; }
"&&" { printf("AND\n"); return AND; }
"|" { printf("BITOR\n"); return BITOR; }
"&" { printf("BITAND\n"); return BITAND; }
"^" { printf("BITXOR\n"); return BITXOR; }
"!" { printf("NOT\n"); return NOT; }
"~" { printf("COM\n"); return COM; }
"<<" { printf("LSH\n"); return LSH; }
">>" { printf("RSH\n"); return RSH; }
"=" { printf("SET\n"); return SET; }
"+=" { printf("SETADD\n"); return SETADD; }
"-=" { printf("SETSUB\n"); return SETSUB; }
"*=" { printf("SETMUL\n"); return SETMUL; }
"/=" { printf("SETDIV\n"); return SETDIV; }
"%=" { printf("SETMOD\n"); return SETMOD; }
"|=" { printf("SETOR\n"); return SETOR; }
"&=" { printf("SETAND\n"); return SETAND; }
"^=" { printf("SETXOR\n"); return SETXOR; }
"<<=" { printf("SETLSH\n"); return SETLSH; }
">>=" { printf("SETRSH\n"); return SETRSH; }

"return" { printf("RETURN\n"); return RETURN; }
"do" { printf("DO\n"); return DO; }
"while" { printf("WHILE\n"); return WHILE; }
"for" { printf("FOR\n"); return FOR; }
"switch" { printf("SWITCH\n"); return SWITCH; }
"case" { printf("CASE\n"); return CASE; }
"default" { printf("DEFAULT\n"); return DEFAULT; }
"if" { printf("IF\n"); return IF; }
"else" { printf("ELSE\n"); return ELSE; }
"continue" { printf("CONTINUE\n"); return CONTINUE; }
"break" { printf("BREAK\n"); return BREAK; }
"goto" { printf("GOTO\n"); return GOTO; }

"unsigned" { printf("UNSIGNED\n"); return UNSIGNED; }
"typedef" { printf("TYPEDEF\n"); return TYPEDEF; }
"struct" { printf("STRUCT\n"); return STRUCT; }
"union" { printf("UNION\n"); return UNION; }
"const" { printf("CONST\n"); return CONST; }
"static" { printf("STATIC\n"); return STATIC; }
"extern" { printf("EXTERN\n"); return EXTERN; }
"auto" { printf("AUTO\n"); return AUTO; }
"register" { printf("REGISTER\n"); return REGISTER; }
"sizeof" { printf("SIZEOF\n"); return SIZEOF; }

{ID} { printf("ID\n"); return ID; }
{INTVAL} { printf("INTVAL\n"); return INTVAL; }
{FLTVAL} { printf("FLTVAL\n"); return FLTVAL; }
{DBLVAL} { printf("DBLVAL\n"); return DBLVAL; }
{STRVAL} { printf("STRVAL\n"); return STRVAL; }
{CHARVAL} { printf("CHARVAL\n"); return CHARVAL; }

[ \t\n\r] {
  /**************************************
   * Reduce any whitespace that is found.
   **************************************/
}

%%

/**************************************************************************
 * Make a call to yylex() recursively. Whenever yylex() is called, it scans
 * tokens from the global input file yyin (which defaults to stdin). It
 * continues until it either reaches an end-of-file (at which point it
 * returns the value 0) or one of its actions executes a return statement.
 *************************************************************************/
int main(int argc, const char* argv[]) {
  while(yylex()) {}
  return 0;
}
