/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-15-2016
*	@brief: Implementa-tion file for driver program-
*/

#include "Test_Stack.h"
#include "Stack.h"

int main(int argc, char** argv)
{
    int testSize = std::stoi(argv[1]);
    Test_Stack tester(testSize);
    tester.runTests();

    std::cout << "\n\n=========================\n";
	std::cout << "  EVALUATING PALINDROME    \n";
	std::cout << "=========================\n\n";
    
    bool done = false;
    do
    {
        int num;
        std::cout << "Input a number: ";

        while (!(std::cin >> num))
        {
            std::cout << "You've entered a non-digit character. Try again.\n\n";
            std::cout << "Input a number: ";
            std::cin.clear(); // clear the input stream
            std::cin.ignore(256, '\n'); // ignore any remaining buffers
        }

        // create stack and check if negative
        Stack<int> numStack;
        bool isNeg = false;

        if (num == 0)
        {
            numStack.push(num);
        }
        else if (num < 0)
        {
            isNeg = true;
            num = num * (-1);
            while (num > 0)
            {
                numStack.push(num % 10);
                num /= 10;
            }
        }
        else
        {
            while (num > 0)
            {
                numStack.push(num % 10);
                num /= 10;
            }
        }

        // reverse the stack and print
        Stack<int> reverseStack = numStack.reverse();
        numStack.print(isNeg);
        std::cout << " backwards is ";
        reverseStack.print(isNeg);

        // check if the input is a palindrome
        bool isPalindrome = numStack.checkPalindrome(reverseStack);
        if (isPalindrome)
            std::cout << "\nIs a palindrome\n";
        else
            std::cout << "\nIs not a palindrome.\n";

        char isUserDone;
        std::cout << "\nDo you want to quit? (Y/N): ";
        std::cin >> isUserDone;

        if (isUserDone == 'y' || isUserDone == 'Y')
            done = true;

    } while(!done);

    return 0;
}
