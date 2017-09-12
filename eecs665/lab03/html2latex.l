%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int list_type;
%}

%option       noyywrap nounput yylineno
%x            COMMENT PARA PRE OL UL LI

%%

"<h1>"[^<]*"</h1>" {
    /**************************************************************************
     * Matched an HTML H1 header so print it out as a LateX section title.
     * The beginning and ending tags need to be stripped off before the
     * title can be formatted in LateX.
     *************************************************************************/
    yytext[yyleng-5] = 0;
    printf( "\\section{%s}", yytext+4 );
}

"<h2>"[^<]*"</h2>" {
    /**************************************************************************
     * Matched an HTML H2 header so print it out as a LateX subsection title.
     * The beginning and ending tags need to be stripped off before the
     * title can be formatted in LateX.
     *************************************************************************/
    yytext[yyleng-5] = 0;
    printf( "\\subsection{%s}", yytext+4 );
}

"<h3>"[^<]*"</h3>" {
    /**************************************************************************
     * Matched an HTML H3 header so print it out as a LateX subsubsection 
     * title. The beginning and ending tags need to be stripped off before 
     * the title can be formatted in LateX.
     *************************************************************************/
    yytext[yyleng-5] = 0;
    printf( "\\subsubsection{%s}", yytext+4 );
}

"<pre>"[^<]*"</pre>" {
    /**************************************************************************
     * Matched an HTML pre-formatted paragraph so print it out as a LateX 
     * verbatim element. The beginning and ending tags need to be stripped 
     * off before the paragraph can be formatted in LateX.
     *************************************************************************/
    yytext[yyleng-6] = 0;
    printf( "\\begin{verbatim}%s\\end{verbatim}", yytext+5 );
}

"<p>" {
    /**************************************************************************
     * Matched an HTML paragraph so print it out as a LateX paragraph.
     * The beginning and ending tags need to be stripped off.
     *************************************************************************/
     BEGIN PARA;
}

"<ul>" {
    /**************************************************************************
     * Matched an HTML unordered list so print it out as a LateX itemize.
     * The beginning and ending tags need to be stripped off.
     *************************************************************************/
     printf( "\\begin{itemize}" );
     BEGIN LI;
}

"<ol>" {
    /**************************************************************************
     * Matched an HTML ordered list so print it out as a LateX enumerate.
     * The beginning and ending tags need to be stripped off. 
     *************************************************************************/
     printf( "\\begin{enumerate}" );
     BEGIN LI;
}

"<!--" {
    /**************************************************************************
     * Matched an HTML comment so print it out as a LateX comment. The
     * beginning and ending tags are stripped off before the LateX comment
     * is emitted.
     *************************************************************************/
    printf( "%% " );
    BEGIN COMMENT;
}

[ \t\n\r]+ {
    /**************************************************************************
     * Matched some whitespace that is not inside of an HTML tag. We
     * just ignore the whitespace.
     *************************************************************************/
}

. {
    /**************************************************************************
     * Matched something that we did not expect to see. Issue an error and
     * exit the program with a failure message.
     *************************************************************************/
    fprintf( stderr, "Unexpected input at line %d: '%s'\n", yylineno, yytext );
    exit( 1 );
}

<LI>"<li>" {
    printf( "\\item ");
}

<LI>"</li>" {
    printf( "\n\n" );
}

<LI>"</ul>" {
    printf( "\\end{itemize}\n" );
    BEGIN 0;
}

<LI>"</ol>" {
    printf( "\\end{enumerate}\n" );
    BEGIN 0;
}

<PARA>"</p>" {
    printf( "\n\n" );
    BEGIN 0;
}

<PARA,LI>"<small>"[^<]*"</small>" {
    yytext[yyleng-8] = 0;
    printf( "{\\scriptsize %s}", yytext+7 );
}

<PARA,LI>"<big>"[^<]*"</big>" {
    yytext[yyleng-6] = 0;
    printf( "{\\Large %s}", yytext+5 );
}

<PARA,LI>"<b>"[^<]*"</b>" {
    yytext[yyleng-4] = 0;
    printf( "\\textbf{%s}", yytext+3 );    
}

<PARA,LI>"<i>"[^<]*"</i>" {
    yytext[yyleng-4] = 0;
    printf( "\\textit{%s}", yytext+3 );
}


<PARA,LI>"<strong>"[^<]*"</strong>" {
    yytext[yyleng-9] = 0;
    printf( "\\textmd{%s}", yytext+8 );
}

<PARA,LI>"<em>"[^<]*"</em>" {
    yytext[yyleng-5] = 0;
    printf( "\\emph{%s}", yytext+4 );
}

<PARA,LI>"<sup>"[^<]*"</sup>" {
    yytext[yyleng-6] = 0;
    printf( "\\textsuperscript{%s}", yytext+5 );
}

<PARA,LI>"<sub>"[^<]*"</sub>" {
    yytext[yyleng-6] = 0;
    printf( "\\textsubscript{%s}", yytext+5 );
}

<COMMENT>"-->" {
    /**************************************************************************
     * Matched an HTML end comment while parsing an HTML comment. Print out
     * a new line and resume normal parsing.
     *************************************************************************/
    printf( "\n" );
    BEGIN 0;
}

<COMMENT>[\n](\r)? {
    /**************************************************************************
     * Matched a newline and possible carriage return while parsing an HTML
     * comment. Print out a new line making sure to mark the next line as a
     * comment as well.
     *************************************************************************/
    printf( "\n%% " );
}

<COMMENT>[ \t] {
    /**************************************************************************
     * Matched some white space while parsing an HTML comment. Print out
     * the white space as is.
     *************************************************************************/
    printf( "%s", yytext );
}

<COMMENT,PARA,LI>. {
    /**************************************************************************
     * Matched any other text while parsing an HTML comment, paragraph, or
     * list item. Print out the text as is.
     *************************************************************************/
    printf( "%s", yytext );
}


%%

/**************************************************************************
 * The main function for the HTML to LateX converter. Here we do three
 * things:
 *  1. Print out the LateX document header.
 *  2. Invoke the lexer to print out the LateX document
 *  3. Print out the LateX footer.
 *************************************************************************/
int main( int argc, const char *argv[] ) {
    printf( "\\documentclass[12pt]{article}\n" );
    printf( "\\usepackage{fixltx2e}\n" );
    printf( "\n" );
    printf( "\\setlength{\\parindent}{0.0in}\n" );
    printf( "\\setlength{\\parskip}{0.125in}\n" );
    printf( "\n\n" );
    printf( "\\begin{document}\n" );
    yylex();
    printf( "\\end{document}\n" );
    return 0;
}
