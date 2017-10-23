# include <stdio.h>
# include <stdlib.h>
# include "cc.h"
# include "semutil.h"
# include "sem.h"
# include "sym.h"


/*
 * backpatch - backpatch list of quadruples starting at p with k
 */
void backpatch(struct sem_rec *p, int k)
{
  // fprintf(stderr, "sem: backpatch not implemented\n");
}

/*
 * bgnstmt - encountered the beginning of a statement
 */
void bgnstmt()
{
  //   fprintf(stderr, "sem: bgnstmt not implemented\n");
}

/*
 * call - procedure invocation
 */
struct sem_rec *call(char *f, struct sem_rec *args)
{
  // fprintf(stderr, "sem: call not implemented\n");
   return ((struct sem_rec *) NULL);
}

/*
 * ccand - logical and
 */
struct sem_rec *ccand(struct sem_rec *e1, int m, struct sem_rec *e2)
{
  // fprintf(stderr, "sem: ccand not implemented\n");
  return ((struct sem_rec *) NULL);
}

/*
 * ccexpr - convert arithmetic expression to logical expression
 */
struct sem_rec *ccexpr(struct sem_rec *e)
{
  // fprintf(stderr, "sem: ccexpr not implemented\n");
  return ((struct sem_rec *) NULL);
}

/*
 * ccnot - logical not
 */
struct sem_rec *ccnot(struct sem_rec *e)
{
  // fprintf(stderr, "sem: ccnot not implemented\n");
  return ((struct sem_rec *) NULL);
}

/*
 * ccor - logical or
 */
struct sem_rec *ccor(struct sem_rec *e1, int m, struct sem_rec *e2)
{
  // fprintf(stderr, "sem: ccor not implemented\n");
  return ((struct sem_rec *) NULL);
}

/*
 * con - constant reference in an expression
 */
struct sem_rec *con(char *x)
{
  // fprintf(stderr, "sem: con not implemented\n");
  return ((struct sem_rec *) NULL);
}

/*
 * dobreak - break statement
 */
void dobreak()
{
  // fprintf(stderr, "sem: dobreak not implemented\n");
}

/*
 * docontinue - continue statement
 */
void docontinue()
{
  // fprintf(stderr, "sem: docontinue not implemented\n");
}

/*
 * dodo - do statement
 */
void dodo(int m1, int m2, struct sem_rec *e, int m3)
{
  // fprintf(stderr, "sem: dodo not implemented\n");
}

/*
 * dofor - for statement
 */
void dofor(int m1, struct sem_rec *e2, int m2, struct sem_rec *n1,
           int m3, struct sem_rec *n2, int m4)
{
  // fprintf(stderr, "sem: dofor not implemented\n");
}

/*
 * dogoto - goto statement
 */
void dogoto(char *id)
{
  // fprintf(stderr, "sem: dogoto not implemented\n");
}

/*
 * doif - one-arm if statement
 */
void doif(struct sem_rec *e, int m1, int m2)
{
  // fprintf(stderr, "sem: doif not implemented\n");
}

/*
 * doifelse - if then else statement
 */
void doifelse(struct sem_rec *e, int m1, struct sem_rec *n,
                         int m2, int m3)
{
  // fprintf(stderr, "sem: doifelse not implemented\n");
}

/*
 * doret - return statement
 */
void doret(struct sem_rec *e)
{
  // fprintf(stderr, "sem: doret not implemented\n");
}

/*
 * dowhile - while statement
 */
void dowhile(int m1, struct sem_rec *e, int m2, struct sem_rec *n,
             int m3)
{
  // fprintf(stderr, "sem: dowhile not implemented\n");
}

/*
 * endloopscope - end the scope for a loop
 */
void endloopscope(int m)
{
  // fprintf(stderr, "sem: endloopscope not implemented\n");
}

/*
 * exprs - form a list of expressions
 */
struct sem_rec *exprs(struct sem_rec *l, struct sem_rec *e)
{
  // fprintf(stderr, "sem: exprs not implemented\n");
   return ((struct sem_rec *) NULL);
}

/*
 * fhead - beginning of function body
 */
void fhead(struct id_entry *p)
{
  fprintf(stderr, "sem: fhead not implemented\n");
  exit(1);
}

/*
 * blockdcl - begining of new block
 */
void blockdcl()
{
  fprintf(stderr, "sem: blockdcl not implemented\n");
  exit(1);
}

/*
 * btail - end of block
 */
void btail()
{
  fprintf(stderr, "sem: btail not implemented\n");
  exit(1);
}

/*
 * fname - function declaration
 */
struct id_entry *fname(int t, char *id)
{
  fprintf(stderr, "sem: fname not implemented\n");
  exit(1);
}

/*
 * ftail - end of function body
 */
void ftail()
{
  fprintf(stderr, "sem: ftail not implemented\n");
  exit(1);
}

/*
 * id - variable reference
 */
struct sem_rec *id(char *x)
{
   struct id_entry *p;

   if ((p = lookup(x, 0)) == NULL) {
      yyerror("undeclared identifier");
      p = install(x, -1);
      p->i_type = T_INT;
      p->i_scope = LOCAL;
      p->i_defined = 1;
   }

   // fprintf(stderr, "sem: id not implemented\n");
   return ((struct sem_rec *) NULL);
}

/*
 * index1 - subscript
 */
struct sem_rec *index1(struct sem_rec *x, struct sem_rec *i)
{
  // fprintf(stderr, "sem: index1 not implemented\n");
   return ((struct sem_rec *) NULL);
}

/*
 * labeldcl - process a label declaration
 */
void labeldcl(char *id)
{
  // fprintf(stderr, "sem: labeldcl not implemented\n");
}

/*
 * m - generate label and return next temporary number
 */
int m()
{
  // fprintf(stderr, "sem: m not implemented\n");
   return (0);
}

/*
 * n - generate goto and return backpatch pointer
 */
struct sem_rec *n()
{
  // fprintf(stderr, "sem: n not implemented\n");
   return ((struct sem_rec *) NULL);
}

/*
 * op1 - unary operators
 */
struct sem_rec *op1(char *op, struct sem_rec *y)
{
  // fprintf(stderr, "sem: op1 not implemented\n");
   return ((struct sem_rec *) NULL);
}

/*
 * op2 - arithmetic operators
 */
struct sem_rec *op2(char *op, struct sem_rec *x, struct sem_rec *y)
{
  // fprintf(stderr, "sem: op2 not implemented\n");
   return ((struct sem_rec *) NULL);
}

/*
 * opb - bitwise operators
 */
struct sem_rec *opb(char *op, struct sem_rec *x, struct sem_rec *y)
{
  // fprintf(stderr, "sem: opb not implemented\n");
   return ((struct sem_rec *) NULL);
}

/*
 * rel - relational operators
 */
struct sem_rec *rel(char *op, struct sem_rec *x, struct sem_rec *y)
{
  // fprintf(stderr, "sem: rel not implemented\n");
   return ((struct sem_rec *) NULL);
}

/*
 * set - assignment operators
 */
struct sem_rec *set(char *op, struct sem_rec *x, struct sem_rec *y)
{
  // fprintf(stderr, "sem: set not implemented\n");
  return ((struct sem_rec *) NULL);
}

/*
 * startloopscope - start the scope for a loop
 */
void startloopscope()
{
  // fprintf(stderr, "sem: startloopscope not implemented\n");
}

/*
 * string - generate code for a string
 */
struct sem_rec *string(char *s)
{
  // fprintf(stderr, "sem: string not implemented\n");
  return ((struct sem_rec *) NULL);
}
