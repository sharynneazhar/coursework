/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 03-22-2016
*	Purpose: main driver
*/

#include <iostream>
#include <string>

#include "Grammar.h"

int main(int argc, char** argv)
{
    std::cout << "\n==================\n\n";
    std::cout << " Grammar Validator    \n";
    std::cout << "\n==================\n\n";

    Grammar Grammar;

    if (argc < 2)
    {
        std::cout << "Please provide an XML file to read.\n\n";
        return 0;
    }

    std::string filename = argv[1];

    try
    {
        Grammar.readFile(filename);
    }
    catch (PVE e)
    {
        std::cerr << e.what();
    }

    std::cout << "\n";

    return 0;
}
