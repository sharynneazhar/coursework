/* To demonstrate some preprocessor issues and compile-time errors */

#include "ndefs.h"
#include <stdio.h>
#include <math.h>

int main()
{
  struct A a;
  int result;

  a.in_var1 = 10;
  a.in_var2 = 2;
  
  result = my_mul(a.in_var1, a.in_var2);
  printf("%d to the power %d = %d\n", a.in_var1, a.in_var2, result);
  
  return 0;
}
