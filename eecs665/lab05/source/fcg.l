%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcg.yacc.h>

#ifndef NDEBUG
#    define DEBUGF(...) printf(__VA_ARGS__)
#else
#    define DEBUGF(...)
#endif

int list_type;
%}

%option       noyywrap nounput yylineno
%x            COMMENT

whitesp       [ \t\n\r]
nonwhitesp    [^ \t\n\r]
ident         [a-zA-Z][a-zA-Z0-9_]*
digit         [0-9]
hexdigit      [0-9a-fA-F]
octdigit      [0-7]

%%

%{
 /**************************************************************************
  * Scan miscellaneous C punctuation.
  *************************************************************************/
%}

"("  { DEBUGF( "(\n" ); return '('; }
")"  { DEBUGF( ")\n" ); return ')'; }
"{"  { DEBUGF( "{\n" ); return '{'; }
"}"  { DEBUGF( "}\n" ); return '}'; }
"["  { DEBUGF( "[\n" ); return '['; }
"]"  { DEBUGF( "]\n" ); return ']'; }
","  { DEBUGF( ",\n" ); return ','; }
";"  { DEBUGF( ";\n" ); return ';'; }

"void"   { DEBUGF("VOID\n"); return VOID; }
"short"  { DEBUGF("SHORT\n"); return SHORT; }
"long"   { DEBUGF("LONG\n"); return LONG; }
"double" { DEBUGF("DOUBLE\n"); return DOUBLE; }
"char"   { DEBUGF("CHAR\n"); return CHAR; }
"int"    { DEBUGF("INT\n"); return INT; }
"float"  { DEBUGF("FLOAT\n"); return FLOAT; }

"=="  { DEBUGF("EQ\n"); return EQ; }
"!="  { DEBUGF("NE\n"); return NE; }
">="  { DEBUGF("GE\n"); return GE; }
"<="  { DEBUGF("LE\n"); return LE; }
">"   { DEBUGF("GT\n"); return GT; }
"<"   { DEBUGF("LT\n"); return LT; }
"+"   { DEBUGF("ADD\n"); return ADD; }
"-"   { DEBUGF("SUB\n"); return SUB; }
"*"   { DEBUGF("MUL\n"); return MUL; }
"/"   { DEBUGF("DIV\n"); return DIV; }
"%"   { DEBUGF("MOD\n"); return MOD; }
"||"  { DEBUGF("OR\n"); return OR; }
"&&"  { DEBUGF("AND\n"); return AND; }
"|"   { DEBUGF("BITOR\n"); return BITOR; }
"&"   { DEBUGF("BITAND\n"); return BITAND; }
"^"   { DEBUGF("BITXOR\n"); return BITXOR; }
"!"   { DEBUGF("NOT\n"); return NOT; }
"~"   { DEBUGF("COM\n"); return COM; }
"<<"  { DEBUGF("LSH\n"); return LSH; }
">>"  { DEBUGF("RSH\n"); return RSH; }
"="   { DEBUGF("SET\n"); return SET; }
"+="  { DEBUGF("SETADD\n"); return SETADD; }
"-="  { DEBUGF("SETSUB\n"); return SETSUB; }
"*="  { DEBUGF("SETMUL\n"); return SETMUL; }
"/="  { DEBUGF("SETDIV\n"); return SETDIV; }
"%="  { DEBUGF("SETMOD\n"); return SETMOD; }
"|="  { DEBUGF("SETOR\n"); return SETOR; }
"&="  { DEBUGF("SETAND\n"); return SETAND; }
"^="  { DEBUGF("SETXOR\n"); return SETXOR; }
"<<=" { DEBUGF("SETLSH\n"); return SETLSH; }
">>=" { DEBUGF("SETRSH\n"); return SETRSH; }

"return"   { DEBUGF("RETURN\n"); return RETURN; }
"do"       { DEBUGF("DO\n"); return DO; }
"while"    { DEBUGF("WHILE\n"); return WHILE; }
"for"      { DEBUGF("FOR\n"); return FOR; }
"switch"   { DEBUGF("SWITCH\n"); return SWITCH; }
"case"     { DEBUGF("CASE\n"); return CASE; }
"default"  { DEBUGF("DEFAULT\n"); return DEFAULT; }
"if"       { DEBUGF("IF\n"); return IF; }
"else"     { DEBUGF("ELSE\n"); return ELSE; }
"continue" { DEBUGF("CONTINUE\n"); return CONTINUE; }
"break"    { DEBUGF("BREAK\n"); return BREAK; }
"goto"     { DEBUGF("GOTO\n"); return GOTO; }

"unsigned" { DEBUGF("UNSIGNED\n"); return UNSIGNED; }
"typedef"  { DEBUGF("TYPEDEF\n"); return TYPEDEF; }
"struct"   { DEBUGF("STRUCT\n"); return STRUCT; }
"union"    { DEBUGF("UNION\n"); return UNION; }
"const"    { DEBUGF("CONST\n"); return CONST; }
"static"   { DEBUGF("STATIC\n"); return STATIC; }
"extern"   { DEBUGF("EXTERN\n"); return EXTERN; }
"auto"     { DEBUGF("AUTO\n"); return AUTO; }
"register" { DEBUGF("REGISTER\n"); return REGISTER; }
"sizeof"   { DEBUGF("SIZEOF\n"); return SIZEOF; }


%{
 /**************************************************************************
  * Scan C identifiers.
  *************************************************************************/
%}

{ident} { DEBUGF( "ID\n" ); yylval.id = strdup(yytext); return ID; }

%{
 /**************************************************************************
  * Scan C literal values including strings, character literals, integer
  * literals, and floating point literals.
  *************************************************************************/
%}

"0x"{hexdigit}+ { DEBUGF( "INTVAL\n" ); return INTVAL; }
"0"{octdigit}*  { DEBUGF( "INTVAL\n" ); return INTVAL; }
[1-9]{digit}*   { DEBUGF( "INTVAL\n" ); return INTVAL; }

%{
 /**************************************************************************
  * Scan a multi-line C comment using left context or a C single line
  * comment to the end of the line.
  *************************************************************************/
%}

"//".*                      { /* ignored */ }

"/*"                        { BEGIN COMMENT; }
<COMMENT>"*/"               { BEGIN 0; }
<COMMENT>([\n](\r)?)|.      { /* ignored */ }

%{
 /**************************************************************************
  * Scan white space in-between tokens and ignore it.
  *************************************************************************/
%}

{whitesp}+ { /* ignored */  }

%{
 /**************************************************************************
  * Anything else that matches is unexpected so print an error.
  *************************************************************************/
%}

. {
    fprintf( stderr, "Unexpected input at line %d: '%s'\n", yylineno, yytext );
    exit( 1 );
}

%%
