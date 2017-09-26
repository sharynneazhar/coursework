/* To demonstrate some preprocessor and compiler issues */

#ifdef CORRECT
#include <stdio.h>
#include <math.h>
#endif

#include "defs.h"
#include "defs2.h"


int main()
{
  struct A a;
  struct B b;
  int result;

  a.in_var1 = 10;
  a.in_var2 = 2;

  result = a.in_var1 * a.in_var2;
  
  printf("var1=%d * var2=%d = %d\n", a.in_var1, a.in_var2, result);
  
  return 0;
}
