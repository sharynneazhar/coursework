/*****************************************************************************
 *
 * File		: simple.c
 * Author	: EECS-678 Staff
 * 
 * Description	: This file contains a couple of warm-up exercises to get you
 * 		  up-to-speed with text editors
 *
 ****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "simple.h"

/*
 * display_onto_stdout
 *
 * Description	: Function to print a string to terminal
 * @str		: Character string to be printed
 */
void display_onto_stdout(const char *str)
{
	printf("%s\n", str);
}

/*
 * find_cosine
 *
 * Description	: Function to calculate cosine of a value
 * @value	: Input value to the cosine function
 *
 * $double	: Return double specifying the cosine of passed in value
 * FIXME	: It's good to keep descriptive comments in your code, but manually formatting long comments is tedious. You can fix this comment by entering command mode and moving your cursor somewhere over the comment. Now type 'gq', followed by one of the commands to move your cursor (e.g.  'j'.  Once done, use column edit command i.e. <c-v> + motion_keys to select the formatted lines and then use <s-i> + tab(s) + spaces to align them with the body of this function header 
 */

double find_cosine(double value)
{
	return cos(value);
}

/*
 * useless_loop
 *
 * Description	: A function which encapsulates a completely useless loop
 * @iterations	: Number of loop iterations
 * FIXME	: The way this code is formatted makes it difficult to read. Use 10== to format
 * 		  this block of code
 */

void useless_loop(int iterations)
{
int i;
	for (i=0; i < iterations; i++) {
		long long t1, t2;
			t1 = random();
				t2 = random();
					t1++;
						t2++;
							}
}

/*
 * print_this_many
 *
 * Description	: A function which prints a specified number of characters
 * @size	: Number of characters to be printed
 * FIXME	: The mark can be used to automate tasks between marks. Mark
 * 		  the line above this function with the letter 'a' (In command
 * 		  mode, place the cursor on the line where the function starts
 * 		  type 'ma'). Mark the line where the function ends with 'mb'.
 * 		  Make sure the cursor is placed on the line where the
 * 		  function ends. Now, press ':', and type 'b, 'a s /^\/\///.
 *
 * 		  This means, "From mark b to mark a replace the string ^\/\/
 * 		  (^ stands for the start of a line, and the backslashes are
 * 		  used to escape the forward slash. The other forward slashes
 * 		  delimit the arguments to the replace command), with an empty
 * 		  string. This might be a good command to map in your
 * 		  .vimrc file.
 *
 * 		  Of course, you could now automatically comment out this
 * 		  piece of code with :'b, 'a s /^/\/\//. To get rid of search
 * 		  highlighting do :set hlsearch! This is mapped to th in the
 * 		  .vimrc.
 *
 * 		  Now, uncomment the print_this_many line in the main function
 * 		  and recompile.
 */

// void print_this_many(int size)
// {
// 	char *bigbuf = malloc(size * sizeof(char));
// 	int i;
// 
// 	for (i=0; i<size; i++) {
// 		bigbuf[i] = 'a' + (i % 26);
// 	}
// 
// 	printf("%s\n", bigbuf);
// }

/*
 * main
 *
 * Description	: Main function for this program
 * @argc	: Number of CLI arguments passed to this program
 * @argv	: List of CLI arguments passed to this program
 * TODO		: Use /^\s to highlight the whitespaces at the start of each
 * 		  line in this program
 * TODO		: There are trailing whitespaces at the end of certain lines
 * 		  in this program. To highlight those instances, use the
 * 		  command /\s$. Try to remove these whitespaces using the
 * 		  command :%s/\s$//gci
 * FIXME	: Correct the indentation in the code below
 */
int main(int argc, char **argv)
{
  double x;

  display_onto_stdout("Simple Program");

  x = find_cosine(COSINE_ONE);
  printf("cosine of 45 is %f\n", x); 

  x = find_cosine(COSINE_TWO);
  printf("cosine of 0 is %f\n", x); 

  useless_loop(LOOP_NUM);

  // print_this_many(PRINT_NUM);

  display_onto_stdout("Done Executing this Simple Program");
  return 0;
}
