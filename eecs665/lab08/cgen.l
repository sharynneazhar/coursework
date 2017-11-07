%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "cgen.yacc.h"

#define YY_USER_ACTION                                  \
    yylloc->first_line = yylloc->last_line = yylineno;  \
    yylloc->first_column = yycolumn+1;                  \
    yylloc->last_column = yycolumn+yyleng;
%}


%option       noyywrap noinput nounput yylineno bison-bridge bison-locations reentrant
whitesp       [ \t\n\r]+
hex           (0x[0-9a-zA-Z]+)|([0-9]*)
dec           ([0-9]+)|([0-9]*)
ident         [a-zA-Z_][a-zA-Z0-9_]*
str           ["].*["]
comment       (\/\/.*|#.*|;.*)


%%

"bt"        { return BT; }
"br"        { return BR; }
"global"    { return GLOBAL; }
"local"     { return LOCAL; }
"param"     { return PARAM; }
"formal"    { return FORMAL; }
"alloc"     { return ALLOC; }
"localloc"  { return LOCALLOC; }
"func"      { return FUNC; }
"fend"      { return FEND; }
"bgnstmt"   { return BGNSTMT; }

":="        { return ASSIGN; }

"==f"       { return FEQ; }
"!=f"       { return FNE; }
"<=f"       { return FLE; }
">=f"       { return FGE; }
"<f"        { return FLT; }
">f"        { return FGT; }
"=f"        { return FASSIGN; }
"+f"        { return FADD; }
"-f"        { return FSUB; }
"*f"        { return FMUL; }
"/f"        { return FDIV; }
"@f"        { return FDEREF; }
"cvf"       { return FCONV; }
"ff"        { return FCALL; }
"argf"      { return FARG; }
"retf"      { return FRET; }
"[]f"       { return FIDX; }

"==i"       { return IEQ; }
"!=i"       { return INE; }
"<=i"       { return ILE; }
">=i"       { return IGE; }
"<i"        { return ILT; }
">i"        { return IGT; }
"=i"        { return IASSIGN; }
"|i"        { return IOR; }
"&i"        { return IAND; }
"∧i"        { return IXOR; }
"<<i"       { return ISHL; }
">>i"       { return ISHR; }
"+i"        { return IADD; }
"-i"        { return ISUB; }
"*i"        { return IMUL; }
"/i"        { return IDIV; }
"%i"        { return IMOD; }
"∼i"        { return IINV; }
"@i"        { return IDEREF; }
"cvi"       { return ICONV; }
"fi"        { return ICALL; }
"argi"      { return IARG; }
"reti"      { return IRET; }
"[]i"       { return IIDX; }


{dec} { 
    // Convert the scanned string into a long long integer value
    // and store the result in the yyval union. The yyval union
    // is created by the parser in calc.y.
    yylval->intval = strtoll(yytext,NULL,0); 

    // Return the INT token to the parser
    return INT; 
}

{hex} { 
    // Convert the scanned string into a long long integer value
    // and store the result in the yyval union. The yyval union
    // is created by the parser in calc.y.
    yylval->intval = strtoll(yytext,NULL,0); 

    // Return the INT token to the parser
    return INT; 
}

{ident} { 
    // Duplicate the scanned string and store it in the yyval union.
    // Since this allocates memory we should technically free the
    // memory some where in the parser. Since this is just an example,
    // however, we don't bother.
    yylval->id = strdup(yytext);

    // Return the ID token to the parser.
    return ID;
}

{str} { 
    // Duplicate the scanned string and store it in the yyval union.
    // Since this allocates memory we should technically free the
    // memory some where in the parser. Since this is just an example,
    // however, we don't bother.
    yylval->str = strdup(yytext);

    // Return the STR token to the parser.
    return STR;
}

{comment} { /* No action and no token */ }
{whitesp} { /* No action and no token */ }
