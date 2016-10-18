/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-22-2016
*	@brief: Implementation file for driver program-
*/

#include <iostream>
#include <string>

#include "Engine.h"
#include "PrecondViolatedExcep.h"

int main(int argc, char** argv)
{
    if (argc < 2)
    {
        // force user to provide a file containing sequence of events
        std::cout << "Please provide a transaction file\n";
        std::cout << "\nFor example: ./prog TransactionFile.txt\n\n";
    }
    else
    {
        std::string filename = argv[1];
        try
        {
            Engine engine;
            engine.run(filename);
        }
        catch (PrecondViolatedExcep& e)
        {
            std::cerr << e.what();
        }
    }

    return 0;
}
