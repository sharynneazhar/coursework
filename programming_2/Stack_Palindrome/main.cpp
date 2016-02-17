/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-15-2016
*	@brief: Implementa-tion file for driver program-
*/

#include "Test_Stack.h"
#include "Stack.h"

#include <string>

template <typename T>
Stack<T> reverse(Stack<T>& stack);

template <typename T>
bool checkPalindrome(const Stack<T>& stack);

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

        Stack<int> numStack;

        if (num > 0)
        {
            while (num > 0)
            {
                numStack.push(num % 10);
                num /= 10;
            }
        }
        else
        {
            std::string numString = std::to_string(num);
            std::cout << numString;
        }

        numStack.print();
        std::cout << " backwards is ";

        Stack<int> reverseStack = reverse(numStack);
        reverseStack.print();

        bool isPalindrome = checkPalindrome(reverseStack);
        std::cout << "\n";
        reverseStack.print();
        if (isPalindrome)
        {
            std::cout << " is a palindrome\n";
        }
        else
        {
            std::cout << " is not a palindrome.\n";
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

template <typename T>
Stack<T> reverse(Stack<T>& stack)
{
    T tempItem;
    Stack<T> tempStack;

    while (!stack.isEmpty())
    {
        tempItem = stack.peek();
        stack.pop();
        tempStack.push(tempItem);
    }

    return tempStack;
}

template <typename T>
bool checkPalindrome(const Stack<T>& stack)
{
    return true;
}
