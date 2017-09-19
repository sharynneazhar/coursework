%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

//Define the tokens that you want to return from your lex as follows
#define ID 256
#define INTVAL 257

%}

%option noyywrap nounput yylineno
%x COMMENT

ID [a-zA-Z][a-zA-Z0-9_]*

%%
"//" {
    /****************************************
    * Matching with Single line comments in our
    * C code
    ******************************************/
    BEGIN COMMENT;
}

<COMMENT>[\n](\r)? {
    /****************************************
    * A new line indicates end of a single
    * line comment in C
    ****************************************/

    BEGIN 0;
}

<COMMENT>[ \t] {
    /****************************************
    * Lexer ignores the comments section
    * by Ignoring anything inside a comment
    ****************************************/
}



<COMMENT>. {
}

{ID} {
    /**************************************
    * If an identifier is matched, then
    * print ID and return a token ID
    **************************************/

    printf("ID\n");
    return ID;
}

%%
int main(int argc, const char* argv[]) {
    /*Make a call to yylex() recursively. Whenever yylex() is called, it scans
    tokens from the global input file yyin (which defaults to stdin). It
    continues until it either reaches an end-of-file (at which point it returns
    the value 0) or one of its actions executes a return statement.*/

    return 0;
}
