/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-01-2016
*	Purpose: main driver program
*/

#include <iostream>
#include "Executive.h"
#include "DriversLicenseRecord.h"
#include "ExecutiveReaderException.h"

int main(int argc, char* argv[])
{
    if (argc <= 1)
    {
        std::cout << "Error. Please provide a file to read.\n";
    }
    else
    {
        std::string file = argv[1];

        try
        {
            Executive Executive(file);
            Executive.run();
        }
        catch (ExecutiveReaderException& e)
        {
            std::cout << e.what();
        }
    }

    return 0;
}
