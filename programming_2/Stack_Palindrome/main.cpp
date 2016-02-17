/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-15-2016
*	@brief: Implementa-tion file for driver program-
*/

#include "Test_Stack.h"
#include "Stack.h"

int main()
{
    int testSize = 20; //std::stoi(argv[1]);
    Test_Stack tester(testSize);
    tester.runTests();

    bool done = false;
    do
    {
        int num;
        std::cout << "\nInput a number: ";
        std::cin >> num;

        // create stack
        Stack<int> numStack;
        while (num > 0)
        {
            numStack.push(num % 10);
            num /= 10;
        }

        numStack.print();
        std::cout << " backwards is ";

        Stack<int> reverseStack = numStack.reverse();
        reverseStack.print();

        bool isPalindrome = numStack.checkPalindrome(reverseStack);

        if (isPalindrome)
        {
            std::cout << "\nIs a palindrome\n";
        }
        else
        {
            std::cout << "\nIs not a palindrome.\n";
        }

        char isUserDone;
        std::cout << "\nDo you want to quit? (Y/N): ";
        std::cin >> isUserDone;

        if (isUserDone == 'y' || isUserDone == 'Y')
        {
            done = true;
        }

    } while(!done);

    return 0;
}
