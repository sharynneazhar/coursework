/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 2015.10.26
*	@brief: Implementation file for msin driver
*/

#include <iostream>
#include <string>

#include "SortDriver.h"
#include "NumberFileDriver.h"
#include "Test.h"

int main (int argc, char** argv)
{
    if (argc < 5)
    {
		std::cout << "Invalid number of arguments\n";
		NumberFileDriver::printHelpMenu();
	}

	std::string mode = (argv[1]);
	if (mode.compare("-create") == 0)
    {
		NumberFileDriver::run(argc, argv);
	}
	else if (mode.compare("-sort") == 0)
    {
		SortDriver::run(argc, argv);
	}
	else
    {
		std::cout<<"Invalid parameters. Check the order of your options\n";
		NumberFileDriver::printHelpMenu();
		SortDriver::printHelpMenu();
	}

    if (argc == 2)
    {
        std::string mode = argv[1];
        if (mode.compare("-test") == 0)
        {
            Test myTester(std::cout);
            myTester.runTests();
        }
        else
        {
                std::cout << "Invalid arguments.\n";
        }
    }

    return 0;
}
