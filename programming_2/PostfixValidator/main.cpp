/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-29-2016
*	Purpose: main driver
*/

#include <iostream>
#include <string>

#include "Stack.h"
#include "PrecondViolatedExcep.h"

// checks if a character is an operator */+-
bool isOperator(char ch);

// checks if a character is a letter
bool isAlpha(char ch);

// converts a postfix to an infix expression
std::string postfixToInfix(const std::string str) throw (PrecondViolatedExcep);

int main()
{
    std::cout << "\n===============================\n\n";
    std::cout << " POSTFIX EXPRESSION VALIDATOR    \n";
    std::cout << "\n===============================\n\n";

    bool isDone = false;
    std::string input;

    do
    {
        std::cout << "Enter a string: ";
        std::getline(cin, input, '\n');

        if (input == "#")
        {
            std::cout << "\n\nExiting...\n\n";
            return true;
        }

        try
        {
            std::cout << "\nThe equivalent infix is "
                      << postfixToInfix(input) << "\n\n";
        }
        catch (PrecondViolatedExcep& e)
        {
            std::cout << e.what();
        }

    } while (!isDone);

    return 0;
}

bool isOperator(char ch)
{
    if (ch == '*' || ch == '/' || ch == '+' || ch == '-')
        return true;

    return false;
}

bool isAlpha(char ch)
{
    if (ch >= 'a' && ch <= 'z')
        return true;
    if (ch >= 'A' && ch <= 'Z')
        return true;

    return false;
}

std::string postfixToInfix(const std::string str) throw (PrecondViolatedExcep)
{
    Stack<std::string> parsedStack;
    std::string postfix = "";
    std::string right = "";
    std::string left = "";
    int counter = 0;

    // parse each character from left to right
    for (unsigned int i = 0; i < str.length(); i++)
    {
        if (!isOperator(str[i]) && !isAlpha(str[i]))
        {
             throw PrecondViolatedExcep("\nInvalid postfix string: encountered an illegal character\n\n");
        }
        else if (isAlpha(str[i]))
        {
            right = str[i];
            parsedStack.push(right);
            counter++;
        }
        else
        {
            try
            {
                // get righthand operand and pop
                right = parsedStack.peek();
                parsedStack.pop();

                // get lefthand operand and pop
                left = parsedStack.peek();
                parsedStack.pop();
            }
            catch (PrecondViolatedExcep& e)
            {
                e.what();
            }

            // concatenate into a string and push to stack
            postfix = "(" + left + str[i] + right + ")";
            parsedStack.push(postfix);
            counter--;
        }
    }

    if (counter < 1 || str.length() < 2)
    {
        throw PrecondViolatedExcep("\nInvalid postfix string: it is missing operands\n\n");
    }
    else if (counter > 1)
    {
        throw PrecondViolatedExcep("\nInvalid postfix string: it is missing one or more operators\n\n");
    }
    else
    {
        return postfix;
    }
}
