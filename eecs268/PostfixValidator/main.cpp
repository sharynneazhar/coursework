/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-29-2016
*	Purpose: main driver
*/

#include <iostream>

#include "Stack.h"
#include "Postfix.h"
#include "PrecondViolatedExcep.h"

int main()
{
    std::cout << "\n===============================\n\n";
    std::cout << " POSTFIX EXPRESSION VALIDATOR    \n";
    std::cout << "\n===============================\n";

    bool isDone = false;
    std::string input;

    do
    {
        // with assumption that expression won't have embedded blanks, otherwise maybe use getline
        std::cout << "\nEnter a string: ";
        std::cin >> input;

        if (input == "#")
        {
            std::cout << "\n\nExiting...\n\n";
            return true;
        }

        try
        {
            Postfix Postfix;
            Postfix.convert(input);
        }
        catch (PrecondViolatedExcep& e)
        {
            std::cerr << e.what();
        }

    } while (!isDone);

    return 0;
}
