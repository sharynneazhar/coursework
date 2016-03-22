/**
*	@file : Postfix.cpp
*	@author : Sharynne Azhar
*	@date : 03-07-2016
*	@brief: Implementation of the postfix class
*/

#include "Postfix.h"

// use default constructor and destructor from compiler

bool Postfix::isOperator(char ch)
{
    if (ch == '*' || ch == '/' || ch == '+' || ch == '-')
        return true;

    return false;
}

bool Postfix::isAlpha(char ch)
{
    if (ch >= 'a' && ch <= 'z')
        return true;
    if (ch >= 'A' && ch <= 'Z')
        return true;

    return false;
}

void Postfix::convert(const std::string str) throw (PrecondViolatedExcep)
{
    std::string right = "";
    std::string left = "";

    int counter = 0; // used to keep track or operand vs operator count

    // parse each character from left to right
    for (unsigned int i = 0; i < str.length(); i++)
    {
        if (!isAlpha(str[i]) && !isOperator(str[i]))
        {
            std::string error = "";
            error = "\nInvalid postfix string: encountered an illegal character \'";
            error += str[i];
            error += "\'\n";
            throw PrecondViolatedExcep(error);
        }
        else if (isOperator(str[i]))
        {
            // get righthand operand and pop
            right = stk.peek();
            stk.pop();

            // get lefthand operand and pop
            left = stk.peek();
            stk.pop();

            // concatenate into a string and push to stack
            infix = "(" + left + str[i] + right + ")";
            stk.push(infix);
            counter--;
        }
        else
        {
            infix = str[i];
            stk.push(infix);
            counter++;
        }
    }

    if (counter > 1)
    {
        throw PrecondViolatedExcep("\nInvalid postfix string: missing one or more operators\n");
    }

    std::cout <<"\nThe equivalent infix is " << infix << "\n";
}
