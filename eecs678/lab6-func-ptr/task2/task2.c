#include <stdio.h>
#include <stdlib.h>

int add (int a, int b);
int subtract (int a, int b);
int multiply (int a, int b);
int divide (int a, int b);

int (*op[4]) (int a, int b);

int main (void)
{
	int operation;

	printf("Operand 'a' : 6 | Operand 'b' : 3\n");
	printf("Specify the operation to perform\n");
	printf("(0 : Add | 1 : Subtract | 2 : Multiply | 3 : Divide): ");
  scanf("%d", &operation);

	// Note: &func and func is the same thing
	op[0] = add;
	op[1] = subtract;
	op[2] = multiply;
	op[3] = divide;

	int result = (*op[operation])(6, 3);

	printf("x = %d\n", result);


	return 0;
}

int add (int a, int b) {
	printf ("Adding 'a' and 'b'\n");
	return a + b;
}

int subtract (int a, int b) {
	printf ("Subtracting 'a' and 'b'\n");
	return a - b;
}

int multiply (int a, int b) {
	printf ("Multiplying 'a' and 'b'\n");
	return a * b;
}

int divide (int a, int b) {
	printf ("Dividing 'a' and 'b'\n");
	return a / b;
}
