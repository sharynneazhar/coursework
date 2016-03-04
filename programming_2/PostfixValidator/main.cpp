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
void postfixToInfix(const std::string str);

int main()
{
    std::cout << "\n===============================\n\n";
    std::cout << " POSTFIX EXPRESSION VALIDATOR    \n";
    std::cout << "\n===============================\n";

    bool isDone = false;
    std::string input;

    do
    {
        std::cout << "\nEnter a string: ";
        std::getline(cin, input, '\n');

        if (input == "#")
        {
            std::cout << "\n\nExiting...\n\n";
            return true;
        }

        try
        {
            postfixToInfix(input);
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

void postfixToInfix(const std::string str)
{
    Stack<std::string> parsedStack;
    std::string infix = "";
    std::string right = "";
    std::string left = "";
    int counter = 0;

    // parse each character from left to right
    for (unsigned int i = 0; i < str.length(); i++)
    {
        if (isAlpha(str[i]))
        {
            right = str[i];
            parsedStack.push(right);
            counter++;
        }
        else if (isOperator(str[i]))
        {
            // get righthand operand and pop
            right = parsedStack.peek();
            parsedStack.pop();

            // get lefthand operand and pop
            left = parsedStack.peek();
            parsedStack.pop();

            // concatenate into a string and push to stack
            infix = "(" + left + str[i] + right + ")";
            parsedStack.push(infix);
            counter--;
        }
        else
        {
             std::cout << "\nInvalid postfix string: encountered an illegal character \'" << str[i] << "\'\n";
             return;
        }
    }

    if (counter > 1)
    {
        std::cout << "\nInvalid postfix string: it is missing one or more operators\n";
    }
    else
    {
        std::cout <<"\nThe equivalent infix is " << infix << "\n";
    }
}
