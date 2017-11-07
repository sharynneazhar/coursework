%{
#include <stdio.h>
#include <function.h>
%}

%locations
%pure_parser
%error-verbose

%union {
    char* id;
    char* str;
    long long intval;
    struct function_t *func;
    struct arg_t *arg;
    struct local_t *loc;
    struct temp_t *tmp;
}

%token BT BR GLOBAL LOCAL PARAM FORMAL ALLOC LOCALLOC FUNC FEND BGNSTMT ASSIGN
%token FEQ FNE FLE FGE FLT FGT FASSIGN FADD FSUB FMUL FDIV FDEREF FCONV
%token FCALL FARG FRET FIDX
%token IEQ INE ILE IGE ILT IGT IASSIGN IOR IAND IXOR ISHL ISHR IADD ISUB IMUL
%token IDIV IMOD IINV IDEREF ICONV ICALL IARG IRET IIDX

%token <intval> INT
%token <id> ID
%token <str> STR

%type <func> fhead
%type <arg> formals
%type <loc> locals
%type <tmp> ops

%start top

%parse-param { void* scanner }
%lex-param { void* scanner }

%{
extern void yyerror( YYLTYPE*, void*, const char* );
extern int yylex( YYSTYPE*, YYLTYPE*, void* );
%}

%%

/******************************************************************************
 ******************************************************************************/
top :               /* empty rule */
    | global top    /* global variables in the file */
    | func top      /* function definitions in the file */

/******************************************************************************
 * Allocate space for a global variable. The GNU as directive ".comm" is
 * useful for this.
 ******************************************************************************/
global : ALLOC ID INT   {
    printf( "    .data\n" );
    printf( "    .globl %s\n", $2 );
    printf( "%s:\n",$2); 
    printf( "    .fill     %lld, 1, 0\n\n", $3 );
}

/******************************************************************************
 * Parse an entire function definition.
 ******************************************************************************/
func  : fhead formals locals stmts FEND {
    printf("    addl       $%d, %%esp\n", STACK_SIZE);
    printf("    pop        %%ebp\n" );
    printf("    ret\n\n");

    for (string_t *str = function->strings; str != NULL; str = str->next) {
        printf( "    .data\n" );
        printf( "    .globl %s\n", str->name );
        printf( "%s:\n", str->name ); 
        printf( "    .asciz    %s\n\n", str->str );
    }

    function_destroy($1);
}

/******************************************************************************
 * Parse the head of a function definition and then print out a function
 * header in assembly as:
 *      .text
 *      .global <name>
 * <name>:
 ******************************************************************************/
fhead : FUNC ID {
    function = function_init($2);
    printf("    .text\n");
    printf("    .globl %s\n", $2);
    printf("%s:\n", $2);
    printf("    push       %%ebp\n");
    printf("    movl       %%esp, %%ebp\n");
    printf("    subl       $%d, %%esp\n", STACK_SIZE);

    $$ = function;
}

/******************************************************************************
 * Parse a list of formal arguments. As the list of formal arguments is
 * parsed, it is added to a linked list inside of the current function_t
 * data structure (named "function").
 ******************************************************************************/
formals : /* empty rule */   { $$ = NULL; }
        | FORMAL INT formals { $$ = function_newarg(function,$2); }

/******************************************************************************
 * Parse a list of local variables. As the list of local variables is
 * parsed, it is added to a linked list inside of the current function_t
 * data structure (named "function").
 ******************************************************************************/
locals  :  /* empty rule */    { $$ = NULL; }
        |  LOCALLOC INT locals { $$ = function_newlocal(function,$2); }

/******************************************************************************
 * Parse a list of statements. Each statement consists of a bgnstmt header
 * followed by a list of operations in that statement.
 ******************************************************************************/
stmts   : /* empty rule */
        | BGNSTMT INT ops stmts

/******************************************************************************
 * Parse a list of operations. Each operation should be printed out to the
 * output as it is parsed. Operands for each operation can be printed out
 * using function_printtemp to print the operand as a value and
 * function_labeltemp to print the operand as a label (values can be
 * used in arithmetic operations while labels can be used in branch
 * operations).
 * 
 * When parsing binary and unary operations we know that the result of the
 * operation is left in register EAX (see documentation for binop and unop).
 * For the ID ASSIGN binop example, we use this fact to make sure that we
 * store the results in the correct local variable using a "movl" instruction.
 * You will need to do something similar for the ID ASSIGN unop case.
 ******************************************************************************/
ops     : /* empty rule */        { $$ = NULL; }
        | ops ID ASSIGN LOCAL INT { $$ = function_getlocal(function,$5,$2); }
        | ops ID ASSIGN GLOBAL ID { $$ = function_getglb(function,$5,$2); }
        | ops ID ASSIGN PARAM INT { $$ = function_getarg(function,$5,$2); }
        | ops ID ASSIGN INT       { $$ = function_getint(function,$4,$2); }
        | ops ID ASSIGN STR       { $$ = function_getstr(function,$4,$2); } 
        | ops ID ASSIGN unop      { $$ = function_gettemp(function,4,$2); }
        | ops ID ASSIGN binop     { $$ = function_gettemp(function,4,$2); 
                                    printf( "    movl       %%eax, " );
                                    function_printtemp(function,$2);
                                    printf( "\n" ); }
        | ops unop                { }
        | ops binop               { } 

/******************************************************************************
 * Parse and generate code for a unary operation. In the example given,
 * the integer return operation is parsed and code is generated to
 * move the correct return value into the EAX register (EAX is the return
 * value register according to cdecl calling conventions).
 ******************************************************************************/
unop    : ISUB ID       { }
        | IINV ID       { }
        | IDEREF ID     { }
        | IARG ID       { }
        | ICALL ID INT  { /* printf( "calling " );
                          function_labeltemp(function,$2);
                          printf( " with %lld arguments\n", $3); */ }
        | IRET ID       { printf( "    movl       " );
                          function_printtemp(function,$2);
                          printf( ", %%eax\n" ); }
        | ICONV ID      { }
        | FSUB ID       { }
        | FDEREF ID     { }
        | FARG ID       { }
        | FCALL ID      { }
        | FRET ID       { }
        | FCONV ID      { }

/******************************************************************************
 * Parse and generate code for a binary operation. In the example given,
 * the integer add operation is parsed and then code is generated using
 * two instructions. The first operation is loaded from memory and place
 * in register EAX. Then the second operation is loaded from memory and
 * added to the value already in EAX.
 *
 * For your solutions, its is easiest to always leave the result of the
 * operation in register EAX, even though this is not necessarily optimal
 * code.
 ******************************************************************************/
binop   : ID IEQ ID     { }
        | ID INE ID     { }
        | ID IGE ID     { }
        | ID ILE ID     { }
        | ID IGT ID     { }
        | ID ILT ID     { }
        | ID IASSIGN ID { }
        | ID IAND ID    { }
        | ID IOR ID     { }
        | ID IXOR ID    { }
        | ID ISHL ID    { }
        | ID ISHR ID    { }
        | ID IADD ID    { printf( "    movl       " );
                          function_printtemp(function,$3);
                          printf( ", %%eax\n" );

                          printf( "    addl       " );
                          function_printtemp(function,$1);
                          printf( ", " );
                          printf( "%%eax\n" ); }
        | ID ISUB ID    { }
        | ID IMUL ID    { }
        | ID IDIV ID    { }
        | ID IMOD ID    { }
        | ID IIDX ID    { }
        | ID FEQ ID     { }
        | ID FNE ID     { }
        | ID FGE ID     { }
        | ID FLE ID     { }
        | ID FGT ID     { }
        | ID FLT ID     { }
        | ID FASSIGN ID { }
        | ID FADD ID    { }
        | ID FSUB ID    { }
        | ID FMUL ID    { }
        | ID FDIV ID    { }
        | ID FIDX ID    { }

%%

/*********************************************************
 * This method is invoked by the parser whenever an
 * error is encountered during parsing. We just print
 * the error to stderr.
 ********************************************************/
void yyerror( YYLTYPE *locp, void* scanner, const char *str ) {
    fprintf( stderr, "On line %d at column %d: %s\n",
             locp->first_line, locp->first_column, str );
}
