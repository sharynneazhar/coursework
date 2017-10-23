#ifndef lint
static const char yysccsid[] = "@(#)yaccpar	1.9 (Berkeley) 02/21/93";
#endif

#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define YYPATCH 20130304

#define YYEMPTY        (-1)
#define yyclearin      (yychar = YYEMPTY)
#define yyerrok        (yyerrflag = 0)
#define YYRECOVERING() (yyerrflag != 0)

#define YYPREFIX "yy"

#define YYPURE 0

#line 2 "cgram.y"
  /* grammar for subset of C. The grammar does not recognize all C syntax:
     For example:
     1. Character literals
     2. Structres
     3. Double constants
     4. Nested paraenthesis of the form "if((x == 1))"
     5. etc.
  */
# include <stdio.h>
# include <stdlib.h>
# include "cc.h"
# include "scan.h"
# include "semutil.h"
# include "sem.h"
# include "sym.h"
#line 19 "cgram.y"
#ifdef YYSTYPE
#undef  YYSTYPE_IS_DECLARED
#define YYSTYPE_IS_DECLARED 1
#endif
#ifndef YYSTYPE_IS_DECLARED
#define YYSTYPE_IS_DECLARED 1
typedef union {
   int inttype;
   char *str_ptr;
   struct sem_rec *rec_ptr;
   struct id_entry *id_ptr;
} YYSTYPE;
#endif /* !YYSTYPE_IS_DECLARED */
#line 49 "y.tab.c"

/* compatibility with bison */
#ifdef YYPARSE_PARAM
/* compatibility with FreeBSD */
# ifdef YYPARSE_PARAM_TYPE
#  define YYPARSE_DECL() yyparse(YYPARSE_PARAM_TYPE YYPARSE_PARAM)
# else
#  define YYPARSE_DECL() yyparse(void *YYPARSE_PARAM)
# endif
#else
# define YYPARSE_DECL() yyparse(void)
#endif

/* Parameters sent to lex. */
#ifdef YYLEX_PARAM
# define YYLEX_DECL() yylex(void *YYLEX_PARAM)
# define YYLEX yylex(YYLEX_PARAM)
#else
# define YYLEX_DECL() yylex(void)
# define YYLEX yylex()
#endif

/* Parameters sent to yyerror. */
#ifndef YYERROR_DECL
#define YYERROR_DECL() yyerror(const char *s)
#endif
#ifndef YYERROR_CALL
#define YYERROR_CALL(msg) yyerror(msg)
#endif

extern int YYPARSE_DECL();

#define ID 257
#define CON 258
#define STR 259
#define CHAR 260
#define ELSE 261
#define FLOAT 262
#define DOUBLE 263
#define FOR 264
#define IF 265
#define INT 266
#define RESERVED 267
#define RETURN 268
#define WHILE 269
#define DO 270
#define CONTINUE 271
#define BREAK 272
#define GOTO 273
#define LVAL 274
#define SET 275
#define SETOR 276
#define SETXOR 277
#define SETAND 278
#define SETLSH 279
#define SETRSH 280
#define SETADD 281
#define SETSUB 282
#define SETMUL 283
#define SETDIV 284
#define SETMOD 285
#define OR 286
#define AND 287
#define BITOR 288
#define BITXOR 289
#define BITAND 290
#define EQ 291
#define NE 292
#define GT 293
#define GE 294
#define LT 295
#define LE 296
#define LSH 297
#define RSH 298
#define ADD 299
#define SUB 300
#define MUL 301
#define DIV 302
#define MOD 303
#define UNARY 304
#define NOT 305
#define COM 306
#define YYERRCODE 256
static const short yylhs[] = {                           -1,
    0,   12,   12,   13,   13,   15,   15,    7,    7,    8,
    8,    8,   10,   10,   10,   10,   14,   16,    9,    9,
   18,   18,   19,   19,   20,   21,   17,   17,   22,   22,
   25,   25,   26,   11,    5,   23,   24,   24,   24,   24,
   24,   24,   24,   24,   24,   24,   24,   24,   24,    6,
    6,    4,    4,    4,    4,    4,    4,    4,    4,    4,
    4,    3,    3,   27,   27,    2,    2,    2,    2,    2,
    2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
    2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
    2,    2,    2,    2,    2,    1,    1,
};
static const short yylen[] = {                            2,
    1,    0,    2,    2,    1,    0,    3,    2,    3,    1,
    3,    4,    1,    1,    1,    1,    3,    4,    2,    1,
    2,    3,    2,    4,    1,    4,    0,    2,    2,    3,
    2,    3,    0,    0,    0,    0,    2,    7,   11,   10,
   11,   16,    2,    2,    3,    2,    3,    1,    1,    0,
    1,    3,    3,    3,    3,    3,    3,    4,    4,    2,
    1,    1,    3,    0,    1,    3,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    3,    2,    2,    2,    1,
    3,    4,    3,    1,    1,    1,    4,
};
static const short yydefred[] = {                         2,
    0,    0,   20,   13,   14,   15,   16,    0,    0,    0,
    3,    5,   27,    4,    0,    0,    0,    0,    8,    0,
    0,    9,   21,    0,    0,    6,    0,   17,   28,    0,
   23,    0,   22,    0,    0,   11,    0,   94,   95,    0,
    0,    0,    0,   34,    0,    0,    0,    0,    0,    0,
   49,   25,    0,    0,    0,    6,   48,   29,    0,    0,
    0,    0,   12,    0,    0,   31,    0,    0,    0,   46,
    0,   34,   33,   43,   44,    0,    0,   87,   88,   89,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   37,    0,    0,   30,   24,    7,    0,   91,
    0,    0,    0,    0,    0,    0,    0,   47,    0,   36,
   45,   93,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   84,   85,   86,    0,   32,   97,    0,   92,   34,   60,
    0,    0,    0,    0,    0,    0,   34,   34,   34,    0,
    0,   26,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   36,   34,    0,    0,    0,    0,   58,    0,
   33,   34,   34,   35,   38,   36,    0,    0,   34,   35,
    0,   35,   36,   34,    0,    0,   34,   40,   34,   34,
   39,   41,   33,   36,   35,   34,   42,
};
static const short yydgoto[] = {                          1,
   54,  116,  112,  117,  189,  177,   61,   19,    9,   62,
   73,    2,   11,   12,   34,   13,   20,   17,   25,   56,
   57,   29,   30,   58,   59,  120,  114,
};
static const short yysindex[] = {                         0,
    0,  -93,    0,    0,    0,    0,    0,  -23,  -36, -248,
    0,    0,    0,    0, -241,  110,  -82,  -61,    0,  -78,
  -61,    0,    0, -241,  -27,    0,  -92,    0,    0,   29,
    0,  -86,    0,  -86,  -50,    0,  -30,    0,    0,   14,
   32,   79,   34,    0,   28,   37, -164, -152,  103,  103,
    0,    0,  103, -122,  733,    0,    0,    0,   52, -241,
   23, -241,    0,  103,   92,    0,  103,   96,  -35,    0,
  829,    0,    0,    0,    0,   50,   25,    0,    0,    0,
  180,  103,  103,  103,  103,  103,  103,  103,  103,  103,
  103,  103,  103,  103,  103,  103,  103,  103,  103,  103,
  103,  103,    0,  -86,  -25,    0,    0,    0,  -87,    0,
 -199,   53, -199,   68,   96,  695,   27,    0,   96,    0,
    0,    0, -199, -199, -199, -199, -199, -199, -199, -199,
 -199, -199, -199, -177, -153, -111, -106, -106, -252, -252,
    0,    0,    0,    3,    0,    0,  103,    0,    0,    0,
  103,  103,  103,  103,  103,  103,    0,    0,    0,   40,
 -139,    0, -199,   96, -199, -199, -199, -199, -199, -199,
   96,   96,    0,    0,   95, -208,   80, -147,    0, -120,
    0,    0,    0,    0,    0,    0,   96,  103,    0,    0,
   77,    0,    0,    0,   83,  124,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,
};
static const short yyrindex[] = {                         0,
    0,  168,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  -32,    0,   75,
  -15,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  -40,    0,    0,  679,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  164,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  112,    0,  141,    0,
    0,    0,    0,    0,    0,    0,  202,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  -17,  679,    0,    0,    0,    0,    0,
   66,    0,  -21,    0,    0,  -28,    0,    0,    0,    0,
    0,    0,  534,  576,  613,  624,  635,  646,  657,  763,
  790,  801,  820,  523,  502,  491,  465,  478,  315,  353,
    0,    0,    0,   75,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   73,  113,  -19,   -6,   -2,    4,   17,   21,
    0,    0,    0,    0,    0,  119,    0,   -4,    0,    6,
    0,    0,    0,    0,    0,    0,    0,  138,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,
};
static const short yygindex[] = {                         0,
  133, 1091,    0, -112, -119,    0,  181,   10,    0,   16,
 1074,    0,    0,    0,  128,    0,  100,    0,    0,    0,
    0, -109,    0,  140,    0, -179,   18,
};
#define YYTABLESIZE 1280
static const short yytable[] = {                         18,
   36,  186,  150,   16,   65,  146,  160,   19,   18,   65,
  161,   10,   61,   33,   65,   21,   32,   10,   18,   65,
   15,   52,   27,  204,   22,   10,   10,   66,   10,   27,
   61,   24,  145,   31,   53,   14,   59,   65,   57,   52,
   26,   27,   63,   10,   55,   34,   28,   60,  100,  101,
  102,  176,   53,   67,   59,   64,   57,   56,  178,  179,
   64,   54,   55,  180,   34,   64,   15,  159,   53,  107,
  194,   68,  196,   72,  191,   56,  190,  157,  158,   54,
  174,  108,   18,  197,   18,  206,   74,   51,   93,   94,
   95,   53,   76,  148,  205,   75,  147,   96,   97,   98,
   99,  100,  101,  102,   77,   27,   62,   27,  121,   62,
   51,   94,   95,   63,   36,   64,   63,  195,   53,   96,
   97,   98,   99,  100,  101,  102,  149,  162,   34,  175,
   34,   53,  110,   36,  182,   53,   95,   70,  183,  158,
  184,  199,   53,   96,   97,   98,   99,  100,  101,  102,
   23,   52,   82,   83,   84,   85,   86,   87,   88,   89,
   90,   91,   92,    3,  200,   35,    4,    1,    5,    6,
   64,   50,    7,    4,   52,    5,    6,   51,   64,    7,
   78,   96,    8,  104,   96,   96,   97,   98,   99,  100,
  101,  102,   98,   99,  100,  101,  102,   36,  106,   96,
   93,   94,   95,  144,   90,  192,    0,   90,    0,   96,
   97,   98,   99,  100,  101,  102,   18,   18,   18,    0,
  122,    0,   90,   18,   18,    0,    0,   18,   18,   18,
   18,   18,   18,   96,    0,    0,    0,    0,    0,   27,
   27,   27,   96,    0,    0,   96,   27,   27,    0,   18,
   27,   27,   27,   27,   27,   27,   90,   61,   61,   18,
   96,    0,   34,   34,   34,   18,   52,   52,    0,   34,
   34,    0,   27,   34,   34,   34,   34,   34,   34,   53,
   53,   59,   27,   57,   57,   37,   38,   39,   27,   55,
   55,    0,   40,   41,   96,   34,   42,   43,   44,   45,
   46,   47,   56,   56,    0,   34,   54,   54,  105,   38,
   39,   34,  157,  158,    0,   40,   41,    0,   48,   42,
   43,   44,   45,   46,   47,  157,  158,    0,   49,    0,
    0,   36,   36,   36,   50,   69,   38,   39,   36,   36,
    0,   48,   36,   36,   36,   36,   36,   36,   69,   38,
   39,   49,   69,   38,   39,   82,    0,   50,   82,   69,
   38,   39,  157,  158,   36,    0,    0,    0,   48,    4,
    0,    5,    6,   82,   36,    7,    0,    0,   49,    0,
   36,   48,    0,    0,   50,   48,    0,    0,    0,    0,
    0,   49,   48,   83,    0,   49,   83,   50,    0,    0,
  115,   50,   49,    0,    0,    0,    0,   82,   50,    0,
    0,   83,    0,    0,    0,   96,   96,   96,   96,   96,
   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,
   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,
   96,   96,   96,   96,    0,   83,    0,    0,    0,   90,
   90,   90,   90,   90,   90,   90,   90,   90,   90,   90,
   90,   90,   90,   90,   90,   90,   90,   93,   94,   95,
    0,    0,    0,    0,    0,    0,   96,   97,   98,   99,
  100,  101,  102,    0,    0,    0,    0,   96,   96,   96,
   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,
   96,   96,   96,   96,   96,   80,    0,    0,   80,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   81,    0,
    0,   81,    0,   80,    0,    0,    0,    0,    0,    0,
    0,   79,    0,    0,   79,    0,   81,    0,    0,    0,
    0,    0,   78,    0,    0,   78,    0,    0,    0,   79,
    0,    0,    0,    0,    0,    0,    0,   80,    0,    0,
   78,    0,    0,   77,    0,    0,   77,    0,    0,    0,
   81,    0,    0,    0,   66,    0,    0,   66,    0,    0,
    0,   77,    0,   79,    0,    0,    0,    0,    0,    0,
    0,    0,   66,    0,   78,    0,    0,    0,    0,    0,
   82,   82,   82,   82,   82,   82,   82,   82,   82,   82,
   82,   82,   82,   82,   82,   77,   67,    0,    0,   67,
    0,    0,    0,    0,    0,    0,   66,    0,    0,    0,
    0,    0,    0,    0,   67,    0,    0,    0,   83,   83,
   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,
   83,   83,   83,   68,    0,    0,   68,    0,    0,    0,
    0,    0,    0,    0,   69,    0,    0,   69,   67,    0,
    0,   68,    0,    0,    0,   70,    0,    0,   70,    0,
    0,    0,   69,    0,    0,    0,   71,    0,    0,   71,
    0,    0,    0,   70,    0,    0,    0,   72,    0,    0,
   72,    0,    0,    0,   71,   68,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   72,   69,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   70,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   96,   71,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   72,
   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,
   80,   80,   80,   81,   81,   81,   81,   81,   81,   81,
   81,   81,   81,   81,   81,   81,   79,   79,   79,   79,
   79,   79,   79,   79,   79,   79,   79,   78,   78,   78,
   78,  103,   78,   78,   78,   78,   78,   78,    0,    0,
    0,    0,    0,   73,    0,    0,   73,    0,   77,   77,
   77,    0,    0,   77,   77,   77,   77,   77,   77,   66,
   66,   73,    0,    0,   66,   66,   66,   66,   66,   66,
   74,    0,    0,   74,    0,    0,    0,    0,    0,    0,
    0,   75,    0,    0,   75,    0,    0,    0,   74,    0,
    0,    0,    0,    0,    0,   73,    0,    0,    0,   75,
   76,   67,   67,   76,    0,    0,   67,   67,   67,   67,
   67,   67,    0,    0,    0,    0,    0,    0,   76,    0,
    0,    0,   74,    0,    0,    0,    0,  118,    0,    0,
    0,    0,    0,   75,    0,    0,    0,    0,   68,   68,
    0,    0,    0,   68,   68,   68,   68,   68,   68,   69,
   69,    0,   76,    0,   69,   69,   69,   69,   69,   69,
   70,   70,    0,    0,    0,   70,   70,   70,   70,   70,
   70,   71,   71,    0,    0,    0,   71,   71,   71,   71,
   71,   71,   72,   72,    0,    0,    0,   72,   72,   72,
   72,   72,   72,   96,   96,   96,   96,   96,   96,   96,
   96,   96,   96,   96,    0,    0,   96,   96,   96,    0,
    0,    0,    0,    0,    0,   96,   96,   96,   96,   96,
   96,   96,   93,   94,   95,  151,  152,  153,  154,  155,
  156,   96,   97,   98,   99,  100,  101,  102,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   93,   94,   95,    0,    0,    0,    0,    0,    0,   96,
   97,   98,   99,  100,  101,  102,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   73,   73,
    0,    0,    0,   73,   73,   73,   73,   73,   73,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   74,   74,    0,    0,    0,
   74,   74,   74,   74,   74,   74,   75,   75,    0,    0,
    0,   75,   75,   75,   75,   75,   75,    0,    0,    0,
    0,    0,    0,    0,    0,   76,   76,    0,    0,    0,
   76,   76,   76,   76,   76,   76,   93,   94,   95,    0,
   55,    0,    0,    0,    0,   96,   97,   98,   99,  100,
  101,  102,   71,    0,    0,    0,    0,    0,    0,   79,
   80,    0,    0,   81,    0,  119,    0,    0,    0,   55,
    0,    0,    0,    0,  109,  111,    0,  113,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  123,  124,  125,  126,  127,  128,  129,  130,
  131,  132,  133,  134,  135,  136,  137,  138,  139,  140,
  141,  142,  143,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  164,    0,    0,    0,    0,    0,    0,    0,
  171,  172,  173,    0,    0,    0,    0,  163,    0,    0,
    0,  165,  166,  167,  168,  169,  170,  181,    0,    0,
    0,    0,    0,  185,    0,  187,  188,    0,    0,    0,
    0,    0,  193,    0,    0,    0,    0,  198,    0,    0,
  201,    0,  202,  203,    0,    0,    0,    0,  113,  207,
};
static const short yycheck[] = {                         40,
   93,  181,  115,   40,   40,   93,  119,   40,  257,   40,
  120,   44,   41,   41,   40,  257,   44,    2,   59,   41,
   44,   41,   40,  203,   15,   41,   59,   58,   44,   91,
   59,   16,   58,   24,   41,   59,   41,   59,   41,   59,
  123,   59,   93,   59,   41,   40,  125,   32,  301,  302,
  303,  164,   59,   40,   59,   91,   59,   41,  171,  172,
   91,   41,   59,  173,   59,   91,   44,   41,   40,   60,
  190,   40,  192,   40,  187,   59,  186,  286,  287,   59,
   41,   59,  123,  193,  125,  205,   59,   59,  288,  289,
  290,   40,  257,   41,  204,   59,   44,  297,  298,  299,
  300,  301,  302,  303,  257,  123,   41,  125,   59,   44,
   59,  289,  290,   41,   40,   91,   44,   41,   40,  297,
  298,  299,  300,  301,  302,  303,   59,  125,  123,  269,
  125,   40,   41,   59,   40,   40,  290,   59,   59,  287,
  261,   59,   40,  297,  298,  299,  300,  301,  302,  303,
   41,  123,  275,  276,  277,  278,  279,  280,  281,  282,
  283,  284,  285,  257,   41,  258,  260,    0,  262,  263,
   59,   59,  266,  260,  123,  262,  263,   59,   41,  266,
   48,   41,    2,   56,   44,  297,  298,  299,  300,  301,
  302,  303,  299,  300,  301,  302,  303,  123,   59,   59,
  288,  289,  290,  104,   41,  188,   -1,   44,   -1,  297,
  298,  299,  300,  301,  302,  303,  257,  258,  259,   -1,
   41,   -1,   59,  264,  265,   -1,   -1,  268,  269,  270,
  271,  272,  273,   93,   -1,   -1,   -1,   -1,   -1,  257,
  258,  259,   41,   -1,   -1,   44,  264,  265,   -1,  290,
  268,  269,  270,  271,  272,  273,   93,  286,  287,  300,
   59,   -1,  257,  258,  259,  306,  286,  287,   -1,  264,
  265,   -1,  290,  268,  269,  270,  271,  272,  273,  286,
  287,  286,  300,  286,  287,  257,  258,  259,  306,  286,
  287,   -1,  264,  265,   93,  290,  268,  269,  270,  271,
  272,  273,  286,  287,   -1,  300,  286,  287,  257,  258,
  259,  306,  286,  287,   -1,  264,  265,   -1,  290,  268,
  269,  270,  271,  272,  273,  286,  287,   -1,  300,   -1,
   -1,  257,  258,  259,  306,  257,  258,  259,  264,  265,
   -1,  290,  268,  269,  270,  271,  272,  273,  257,  258,
  259,  300,  257,  258,  259,   41,   -1,  306,   44,  257,
  258,  259,  286,  287,  290,   -1,   -1,   -1,  290,  260,
   -1,  262,  263,   59,  300,  266,   -1,   -1,  300,   -1,
  306,  290,   -1,   -1,  306,  290,   -1,   -1,   -1,   -1,
   -1,  300,  290,   41,   -1,  300,   44,  306,   -1,   -1,
  305,  306,  300,   -1,   -1,   -1,   -1,   93,  306,   -1,
   -1,   59,   -1,   -1,   -1,  275,  276,  277,  278,  279,
  280,  281,  282,  283,  284,  285,  286,  287,  288,  289,
  290,  291,  292,  293,  294,  295,  296,  297,  298,  299,
  300,  301,  302,  303,   -1,   93,   -1,   -1,   -1,  286,
  287,  288,  289,  290,  291,  292,  293,  294,  295,  296,
  297,  298,  299,  300,  301,  302,  303,  288,  289,  290,
   -1,   -1,   -1,   -1,   -1,   -1,  297,  298,  299,  300,
  301,  302,  303,   -1,   -1,   -1,   -1,  286,  287,  288,
  289,  290,  291,  292,  293,  294,  295,  296,  297,  298,
  299,  300,  301,  302,  303,   41,   -1,   -1,   44,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   41,   -1,
   -1,   44,   -1,   59,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   41,   -1,   -1,   44,   -1,   59,   -1,   -1,   -1,
   -1,   -1,   41,   -1,   -1,   44,   -1,   -1,   -1,   59,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,   -1,   -1,
   59,   -1,   -1,   41,   -1,   -1,   44,   -1,   -1,   -1,
   93,   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,   -1,
   -1,   59,   -1,   93,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   59,   -1,   93,   -1,   -1,   -1,   -1,   -1,
  286,  287,  288,  289,  290,  291,  292,  293,  294,  295,
  296,  297,  298,  299,  300,   93,   41,   -1,   -1,   44,
   -1,   -1,   -1,   -1,   -1,   -1,   93,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   59,   -1,   -1,   -1,  286,  287,
  288,  289,  290,  291,  292,  293,  294,  295,  296,  297,
  298,  299,  300,   41,   -1,   -1,   44,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,   93,   -1,
   -1,   59,   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,
   -1,   -1,   59,   -1,   -1,   -1,   41,   -1,   -1,   44,
   -1,   -1,   -1,   59,   -1,   -1,   -1,   41,   -1,   -1,
   44,   -1,   -1,   -1,   59,   93,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   59,   93,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   59,   93,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,
  286,  287,  288,  289,  290,  291,  292,  293,  294,  295,
  296,  297,  298,  286,  287,  288,  289,  290,  291,  292,
  293,  294,  295,  296,  297,  298,  286,  287,  288,  289,
  290,  291,  292,  293,  294,  295,  296,  286,  287,  288,
  289,   59,  291,  292,  293,  294,  295,  296,   -1,   -1,
   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,  286,  287,
  288,   -1,   -1,  291,  292,  293,  294,  295,  296,  286,
  287,   59,   -1,   -1,  291,  292,  293,  294,  295,  296,
   41,   -1,   -1,   44,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   41,   -1,   -1,   44,   -1,   -1,   -1,   59,   -1,
   -1,   -1,   -1,   -1,   -1,   93,   -1,   -1,   -1,   59,
   41,  286,  287,   44,   -1,   -1,  291,  292,  293,  294,
  295,  296,   -1,   -1,   -1,   -1,   -1,   -1,   59,   -1,
   -1,   -1,   93,   -1,   -1,   -1,   -1,   59,   -1,   -1,
   -1,   -1,   -1,   93,   -1,   -1,   -1,   -1,  286,  287,
   -1,   -1,   -1,  291,  292,  293,  294,  295,  296,  286,
  287,   -1,   93,   -1,  291,  292,  293,  294,  295,  296,
  286,  287,   -1,   -1,   -1,  291,  292,  293,  294,  295,
  296,  286,  287,   -1,   -1,   -1,  291,  292,  293,  294,
  295,  296,  286,  287,   -1,   -1,   -1,  291,  292,  293,
  294,  295,  296,  275,  276,  277,  278,  279,  280,  281,
  282,  283,  284,  285,   -1,   -1,  288,  289,  290,   -1,
   -1,   -1,   -1,   -1,   -1,  297,  298,  299,  300,  301,
  302,  303,  288,  289,  290,  291,  292,  293,  294,  295,
  296,  297,  298,  299,  300,  301,  302,  303,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  288,  289,  290,   -1,   -1,   -1,   -1,   -1,   -1,  297,
  298,  299,  300,  301,  302,  303,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  286,  287,
   -1,   -1,   -1,  291,  292,  293,  294,  295,  296,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  286,  287,   -1,   -1,   -1,
  291,  292,  293,  294,  295,  296,  286,  287,   -1,   -1,
   -1,  291,  292,  293,  294,  295,  296,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  286,  287,   -1,   -1,   -1,
  291,  292,  293,  294,  295,  296,  288,  289,  290,   -1,
   30,   -1,   -1,   -1,   -1,  297,  298,  299,  300,  301,
  302,  303,   42,   -1,   -1,   -1,   -1,   -1,   -1,   49,
   50,   -1,   -1,   53,   -1,   72,   -1,   -1,   -1,   59,
   -1,   -1,   -1,   -1,   64,   65,   -1,   67,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   82,   83,   84,   85,   86,   87,   88,   89,
   90,   91,   92,   93,   94,   95,   96,   97,   98,   99,
  100,  101,  102,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  149,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  157,  158,  159,   -1,   -1,   -1,   -1,  147,   -1,   -1,
   -1,  151,  152,  153,  154,  155,  156,  174,   -1,   -1,
   -1,   -1,   -1,  180,   -1,  182,  183,   -1,   -1,   -1,
   -1,   -1,  189,   -1,   -1,   -1,   -1,  194,   -1,   -1,
  197,   -1,  199,  200,   -1,   -1,   -1,   -1,  188,  206,
};
#define YYFINAL 1
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 306
#if YYDEBUG
static const char *yyname[] = {

"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,"'('","')'",0,0,"','",0,0,0,0,0,0,0,0,0,0,0,0,0,"':'","';'",0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'['",0,"']'",0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'{'",0,"'}'",0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,"ID","CON","STR","CHAR","ELSE","FLOAT","DOUBLE","FOR","IF","INT","RESERVED",
"RETURN","WHILE","DO","CONTINUE","BREAK","GOTO","LVAL","SET","SETOR","SETXOR",
"SETAND","SETLSH","SETRSH","SETADD","SETSUB","SETMUL","SETDIV","SETMOD","OR",
"AND","BITOR","BITXOR","BITAND","EQ","NE","GT","GE","LT","LE","LSH","RSH","ADD",
"SUB","MUL","DIV","MOD","UNARY","NOT","COM",
};
static const char *yyrule[] = {
"$accept : prog",
"prog : externs",
"externs :",
"externs : externs extern",
"extern : dcl ';'",
"extern : func",
"dcls :",
"dcls : dcls dcl ';'",
"dcl : type dclr",
"dcl : dcl ',' dclr",
"dclr : ID",
"dclr : ID '[' ']'",
"dclr : ID '[' CON ']'",
"type : CHAR",
"type : FLOAT",
"type : DOUBLE",
"type : INT",
"func : fhead stmts '}'",
"fhead : fname fargs '{' dcls",
"fname : type ID",
"fname : ID",
"fargs : '(' ')'",
"fargs : '(' args ')'",
"args : type dclr",
"args : args ',' type dclr",
"bhead : '{'",
"block : bhead dcls stmts '}'",
"stmts :",
"stmts : stmts lblstmt",
"lblstmt : b stmt",
"lblstmt : b labels stmt",
"labels : ID ':'",
"labels : labels ID ':'",
"s :",
"m :",
"n :",
"b :",
"stmt : expr ';'",
"stmt : IF '(' cexpr ')' m lblstmt m",
"stmt : IF '(' cexpr ')' m lblstmt ELSE n m lblstmt m",
"stmt : WHILE '(' m cexpr ')' m s lblstmt n m",
"stmt : DO m s lblstmt WHILE '(' m cexpr ')' ';' m",
"stmt : FOR '(' expro ';' m cexpro ';' m expro n ')' m s lblstmt n m",
"stmt : CONTINUE ';'",
"stmt : BREAK ';'",
"stmt : GOTO ID ';'",
"stmt : RETURN ';'",
"stmt : RETURN expr ';'",
"stmt : block",
"stmt : ';'",
"cexpro :",
"cexpro : cexpr",
"cexpr : expr EQ expr",
"cexpr : expr NE expr",
"cexpr : expr LE expr",
"cexpr : expr GE expr",
"cexpr : expr LT expr",
"cexpr : expr GT expr",
"cexpr : cexpr AND m cexpr",
"cexpr : cexpr OR m cexpr",
"cexpr : NOT cexpr",
"cexpr : expr",
"exprs : expr",
"exprs : exprs ',' expr",
"expro :",
"expro : expr",
"expr : lval SET expr",
"expr : lval SETOR expr",
"expr : lval SETXOR expr",
"expr : lval SETAND expr",
"expr : lval SETLSH expr",
"expr : lval SETRSH expr",
"expr : lval SETADD expr",
"expr : lval SETSUB expr",
"expr : lval SETMUL expr",
"expr : lval SETDIV expr",
"expr : lval SETMOD expr",
"expr : expr BITOR expr",
"expr : expr BITXOR expr",
"expr : expr BITAND expr",
"expr : expr LSH expr",
"expr : expr RSH expr",
"expr : expr ADD expr",
"expr : expr SUB expr",
"expr : expr MUL expr",
"expr : expr DIV expr",
"expr : expr MOD expr",
"expr : BITAND lval",
"expr : SUB expr",
"expr : COM expr",
"expr : lval",
"expr : ID '(' ')'",
"expr : ID '(' exprs ')'",
"expr : '(' expr ')'",
"expr : CON",
"expr : STR",
"lval : ID",
"lval : ID '[' expr ']'",

};
#endif

int      yydebug;
int      yynerrs;

int      yyerrflag;
int      yychar;
YYSTYPE  yyval;
YYSTYPE  yylval;

/* define the initial stack-sizes */
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH  YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 10000
#define YYMAXDEPTH  500
#endif
#endif

#define YYINITSTACKSIZE 500

typedef struct {
    unsigned stacksize;
    short    *s_base;
    short    *s_mark;
    short    *s_last;
    YYSTYPE  *l_base;
    YYSTYPE  *l_mark;
} YYSTACKDATA;
/* variables for the parser stack */
static YYSTACKDATA yystack;
#line 211 "cgram.y"
# include <stdio.h>

extern int lineno;

/*
 * main - read a program, and parse it
 */
int main(int argc, char **argv)
{

   enterblock();
   initlex();
   enterblock();
   if (yyparse())
      yyerror("syntax error");
   leaveblock();
   exit(0);
}

/*
 * yyerror - issue error message
 */
void yyerror(char msg[])
{
   fprintf(stderr, " %s.  Line %d\n", msg, lineno);
}
#line 681 "y.tab.c"

#if YYDEBUG
#include <stdio.h>		/* needed for printf */
#endif

#include <stdlib.h>	/* needed for malloc, etc */
#include <string.h>	/* needed for memset */

/* allocate initial stack or double stack size, up to YYMAXDEPTH */
static int yygrowstack(YYSTACKDATA *data)
{
    int i;
    unsigned newsize;
    short *newss;
    YYSTYPE *newvs;

    if ((newsize = data->stacksize) == 0)
        newsize = YYINITSTACKSIZE;
    else if (newsize >= YYMAXDEPTH)
        return -1;
    else if ((newsize *= 2) > YYMAXDEPTH)
        newsize = YYMAXDEPTH;

    i = (int) (data->s_mark - data->s_base);
    newss = (short *)realloc(data->s_base, newsize * sizeof(*newss));
    if (newss == 0)
        return -1;

    data->s_base = newss;
    data->s_mark = newss + i;

    newvs = (YYSTYPE *)realloc(data->l_base, newsize * sizeof(*newvs));
    if (newvs == 0)
        return -1;

    data->l_base = newvs;
    data->l_mark = newvs + i;

    data->stacksize = newsize;
    data->s_last = data->s_base + newsize - 1;
    return 0;
}

#if YYPURE || defined(YY_NO_LEAKS)
static void yyfreestack(YYSTACKDATA *data)
{
    free(data->s_base);
    free(data->l_base);
    memset(data, 0, sizeof(*data));
}
#else
#define yyfreestack(data) /* nothing */
#endif

#define YYABORT  goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR  goto yyerrlab

int
YYPARSE_DECL()
{
    int yym, yyn, yystate;
#if YYDEBUG
    const char *yys;

    if ((yys = getenv("YYDEBUG")) != 0)
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = YYEMPTY;
    yystate = 0;

#if YYPURE
    memset(&yystack, 0, sizeof(yystack));
#endif

    if (yystack.s_base == NULL && yygrowstack(&yystack)) goto yyoverflow;
    yystack.s_mark = yystack.s_base;
    yystack.l_mark = yystack.l_base;
    yystate = 0;
    *yystack.s_mark = 0;

yyloop:
    if ((yyn = yydefred[yystate]) != 0) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = YYLEX) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yystack.s_mark >= yystack.s_last && yygrowstack(&yystack))
        {
            goto yyoverflow;
        }
        yystate = yytable[yyn];
        *++yystack.s_mark = yytable[yyn];
        *++yystack.l_mark = yylval;
        yychar = YYEMPTY;
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;

    yyerror("syntax error");

    goto yyerrlab;

yyerrlab:
    ++yynerrs;

yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yystack.s_mark]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yystack.s_mark, yytable[yyn]);
#endif
                if (yystack.s_mark >= yystack.s_last && yygrowstack(&yystack))
                {
                    goto yyoverflow;
                }
                yystate = yytable[yyn];
                *++yystack.s_mark = yytable[yyn];
                *++yystack.l_mark = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yystack.s_mark);
#endif
                if (yystack.s_mark <= yystack.s_base) goto yyabort;
                --yystack.s_mark;
                --yystack.l_mark;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = YYEMPTY;
        goto yyloop;
    }

yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    if (yym)
        yyval = yystack.l_mark[1-yym];
    else
        memset(&yyval, 0, sizeof yyval);
    switch (yyn)
    {
case 1:
#line 45 "cgram.y"
	{}
break;
case 2:
#line 48 "cgram.y"
	{}
break;
case 3:
#line 49 "cgram.y"
	{}
break;
case 4:
#line 52 "cgram.y"
	{}
break;
case 5:
#line 53 "cgram.y"
	{}
break;
case 6:
#line 56 "cgram.y"
	{}
break;
case 7:
#line 57 "cgram.y"
	{}
break;
case 8:
#line 60 "cgram.y"
	{ yyval.id_ptr = dcl(yystack.l_mark[0].id_ptr, yystack.l_mark[-1].inttype, 0); }
break;
case 9:
#line 61 "cgram.y"
	{ yyval.id_ptr = dcl(yystack.l_mark[0].id_ptr, yystack.l_mark[-2].id_ptr->i_type&~T_ARRAY, 0); }
break;
case 10:
#line 64 "cgram.y"
	{ yyval.id_ptr = dclr(yystack.l_mark[0].str_ptr, 0, 1); }
break;
case 11:
#line 65 "cgram.y"
	{ yyval.id_ptr = dclr(yystack.l_mark[-2].str_ptr, T_ARRAY, 1); }
break;
case 12:
#line 66 "cgram.y"
	{ yyval.id_ptr = dclr(yystack.l_mark[-3].str_ptr, T_ARRAY, atoi(yystack.l_mark[-1].str_ptr)); }
break;
case 13:
#line 69 "cgram.y"
	{ yyval.inttype = T_INT; }
break;
case 14:
#line 70 "cgram.y"
	{ yyval.inttype = T_DOUBLE; }
break;
case 15:
#line 71 "cgram.y"
	{ yyval.inttype = T_DOUBLE; }
break;
case 16:
#line 72 "cgram.y"
	{ yyval.inttype = T_INT; }
break;
case 17:
#line 75 "cgram.y"
	{ ftail(); }
break;
case 18:
#line 78 "cgram.y"
	{ fhead(yystack.l_mark[-3].id_ptr); }
break;
case 19:
#line 81 "cgram.y"
	{ yyval.id_ptr = fname(yystack.l_mark[-1].inttype, yystack.l_mark[0].str_ptr); }
break;
case 20:
#line 82 "cgram.y"
	{ yyval.id_ptr = fname(T_INT, yystack.l_mark[0].str_ptr); }
break;
case 21:
#line 85 "cgram.y"
	{ enterblock(); }
break;
case 22:
#line 86 "cgram.y"
	{ enterblock(); }
break;
case 23:
#line 89 "cgram.y"
	{ dcl(yystack.l_mark[0].id_ptr, yystack.l_mark[-1].inttype, PARAM); }
break;
case 24:
#line 90 "cgram.y"
	{ dcl(yystack.l_mark[0].id_ptr, yystack.l_mark[-1].inttype, PARAM); }
break;
case 25:
#line 93 "cgram.y"
	{ blockdcl(); }
break;
case 26:
#line 95 "cgram.y"
	{ btail(); }
break;
case 27:
#line 98 "cgram.y"
	{ }
break;
case 28:
#line 99 "cgram.y"
	{ }
break;
case 29:
#line 102 "cgram.y"
	{ }
break;
case 30:
#line 103 "cgram.y"
	{ }
break;
case 31:
#line 106 "cgram.y"
	{ labeldcl(yystack.l_mark[-1].str_ptr); }
break;
case 32:
#line 107 "cgram.y"
	{ labeldcl(yystack.l_mark[-1].str_ptr); }
break;
case 33:
#line 110 "cgram.y"
	{ startloopscope(); }
break;
case 34:
#line 113 "cgram.y"
	{ yyval.inttype = m(); }
break;
case 35:
#line 116 "cgram.y"
	{ yyval.rec_ptr = n(); }
break;
case 36:
#line 119 "cgram.y"
	{ bgnstmt(); }
break;
case 37:
#line 123 "cgram.y"
	{ }
break;
case 38:
#line 125 "cgram.y"
	{ doif(yystack.l_mark[-4].rec_ptr, yystack.l_mark[-2].inttype, yystack.l_mark[0].inttype); }
break;
case 39:
#line 127 "cgram.y"
	{ doifelse(yystack.l_mark[-8].rec_ptr, yystack.l_mark[-6].inttype, yystack.l_mark[-3].rec_ptr, yystack.l_mark[-2].inttype, yystack.l_mark[0].inttype); }
break;
case 40:
#line 129 "cgram.y"
	{ dowhile(yystack.l_mark[-7].inttype, yystack.l_mark[-6].rec_ptr, yystack.l_mark[-4].inttype, yystack.l_mark[-1].rec_ptr, yystack.l_mark[0].inttype); }
break;
case 41:
#line 131 "cgram.y"
	{ dodo(yystack.l_mark[-9].inttype, yystack.l_mark[-4].inttype, yystack.l_mark[-3].rec_ptr, yystack.l_mark[0].inttype); }
break;
case 42:
#line 133 "cgram.y"
	{ dofor(yystack.l_mark[-11].inttype, yystack.l_mark[-10].rec_ptr, yystack.l_mark[-8].inttype, yystack.l_mark[-6].rec_ptr, yystack.l_mark[-4].inttype, yystack.l_mark[-1].rec_ptr, yystack.l_mark[0].inttype); }
break;
case 43:
#line 135 "cgram.y"
	{ docontinue(); }
break;
case 44:
#line 137 "cgram.y"
	{ dobreak(); }
break;
case 45:
#line 139 "cgram.y"
	{ dogoto(yystack.l_mark[-1].str_ptr); }
break;
case 46:
#line 141 "cgram.y"
	{ doret((struct sem_rec *) NULL); }
break;
case 47:
#line 143 "cgram.y"
	{ doret(yystack.l_mark[-1].rec_ptr); }
break;
case 48:
#line 145 "cgram.y"
	{ }
break;
case 49:
#line 147 "cgram.y"
	{ }
break;
case 50:
#line 150 "cgram.y"
	{ yyval.rec_ptr = node(0, 0, n(), 0); }
break;
case 51:
#line 151 "cgram.y"
	{}
break;
case 52:
#line 154 "cgram.y"
	{ yyval.rec_ptr = rel("==", yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 53:
#line 155 "cgram.y"
	{ yyval.rec_ptr = rel("!=", yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 54:
#line 156 "cgram.y"
	{ yyval.rec_ptr = rel("<=", yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 55:
#line 157 "cgram.y"
	{ yyval.rec_ptr = rel(">=", yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 56:
#line 158 "cgram.y"
	{ yyval.rec_ptr = rel("<",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 57:
#line 159 "cgram.y"
	{ yyval.rec_ptr = rel(">",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 58:
#line 160 "cgram.y"
	{ yyval.rec_ptr = ccand(yystack.l_mark[-3].rec_ptr, yystack.l_mark[-1].inttype, yystack.l_mark[0].rec_ptr); }
break;
case 59:
#line 161 "cgram.y"
	{ yyval.rec_ptr = ccor(yystack.l_mark[-3].rec_ptr, yystack.l_mark[-1].inttype, yystack.l_mark[0].rec_ptr); }
break;
case 60:
#line 162 "cgram.y"
	{ yyval.rec_ptr = ccnot(yystack.l_mark[0].rec_ptr); }
break;
case 61:
#line 163 "cgram.y"
	{ yyval.rec_ptr = ccexpr(yystack.l_mark[0].rec_ptr); }
break;
case 62:
#line 166 "cgram.y"
	{ yyval.rec_ptr = yystack.l_mark[0].rec_ptr; }
break;
case 63:
#line 167 "cgram.y"
	{ yyval.rec_ptr = exprs(yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 64:
#line 170 "cgram.y"
	{}
break;
case 65:
#line 171 "cgram.y"
	{}
break;
case 66:
#line 174 "cgram.y"
	{ yyval.rec_ptr = set("",   yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 67:
#line 175 "cgram.y"
	{ yyval.rec_ptr = set("|",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 68:
#line 176 "cgram.y"
	{ yyval.rec_ptr = set("^",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 69:
#line 177 "cgram.y"
	{ yyval.rec_ptr = set("&",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 70:
#line 178 "cgram.y"
	{ yyval.rec_ptr = set("<<", yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 71:
#line 179 "cgram.y"
	{ yyval.rec_ptr = set(">>", yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 72:
#line 180 "cgram.y"
	{ yyval.rec_ptr = set("+",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 73:
#line 181 "cgram.y"
	{ yyval.rec_ptr = set("-",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 74:
#line 182 "cgram.y"
	{ yyval.rec_ptr = set("*",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 75:
#line 183 "cgram.y"
	{ yyval.rec_ptr = set("/",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 76:
#line 184 "cgram.y"
	{ yyval.rec_ptr = set("%",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 77:
#line 185 "cgram.y"
	{ yyval.rec_ptr = opb("|",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 78:
#line 186 "cgram.y"
	{ yyval.rec_ptr = opb("^",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 79:
#line 187 "cgram.y"
	{ yyval.rec_ptr = opb("&",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 80:
#line 188 "cgram.y"
	{ yyval.rec_ptr = opb("<<", yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 81:
#line 189 "cgram.y"
	{ yyval.rec_ptr = opb(">>", yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 82:
#line 190 "cgram.y"
	{ yyval.rec_ptr = op2("+",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 83:
#line 191 "cgram.y"
	{ yyval.rec_ptr = op2("-",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 84:
#line 192 "cgram.y"
	{ yyval.rec_ptr = op2("*",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 85:
#line 193 "cgram.y"
	{ yyval.rec_ptr = op2("/",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 86:
#line 194 "cgram.y"
	{ yyval.rec_ptr = op2("%",  yystack.l_mark[-2].rec_ptr, yystack.l_mark[0].rec_ptr); }
break;
case 87:
#line 196 "cgram.y"
	{ yyval.rec_ptr = yystack.l_mark[0].rec_ptr; }
break;
case 88:
#line 197 "cgram.y"
	{ yyval.rec_ptr = op1("-",      yystack.l_mark[0].rec_ptr); }
break;
case 89:
#line 198 "cgram.y"
	{ yyval.rec_ptr = op1("~",      yystack.l_mark[0].rec_ptr); }
break;
case 90:
#line 199 "cgram.y"
	{ yyval.rec_ptr = op1("@",      yystack.l_mark[0].rec_ptr); }
break;
case 91:
#line 200 "cgram.y"
	{ yyval.rec_ptr = call(yystack.l_mark[-2].str_ptr, (struct sem_rec *) NULL); }
break;
case 92:
#line 201 "cgram.y"
	{ yyval.rec_ptr = call(yystack.l_mark[-3].str_ptr, yystack.l_mark[-1].rec_ptr); }
break;
case 93:
#line 202 "cgram.y"
	{ yyval.rec_ptr = yystack.l_mark[-1].rec_ptr; }
break;
case 94:
#line 203 "cgram.y"
	{ yyval.rec_ptr = con(yystack.l_mark[0].str_ptr); }
break;
case 95:
#line 204 "cgram.y"
	{ yyval.rec_ptr = string(yystack.l_mark[0].str_ptr); }
break;
case 96:
#line 207 "cgram.y"
	{ yyval.rec_ptr = id(yystack.l_mark[0].str_ptr); }
break;
case 97:
#line 208 "cgram.y"
	{ yyval.rec_ptr = index1(id(yystack.l_mark[-3].str_ptr), yystack.l_mark[-1].rec_ptr); }
break;
#line 1275 "y.tab.c"
    }
    yystack.s_mark -= yym;
    yystate = *yystack.s_mark;
    yystack.l_mark -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yystack.s_mark = YYFINAL;
        *++yystack.l_mark = yyval;
        if (yychar < 0)
        {
            if ((yychar = YYLEX) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yystack.s_mark, yystate);
#endif
    if (yystack.s_mark >= yystack.s_last && yygrowstack(&yystack))
    {
        goto yyoverflow;
    }
    *++yystack.s_mark = (short) yystate;
    *++yystack.l_mark = yyval;
    goto yyloop;

yyoverflow:
    yyerror("yacc stack overflow");

yyabort:
    yyfreestack(&yystack);
    return (1);

yyaccept:
    yyfreestack(&yystack);
    return (0);
}
