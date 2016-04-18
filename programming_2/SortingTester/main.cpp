/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 04-11-2016
*	Purpose: main driver used to run the algorithm tester
*/

#include <iostream>
#include <string>

#include "SortDriver.h"

int main(int argc, char** argv) {

    if (argc < 3) {
        std::cerr << "ERROR: Invalid number of arguments\n";
        SortDriver::printHelpMenu();
    }

    std::string mode = argv[1];
    if (mode.compare("-a") == 0) {
        SortDriver::runAnalysis(argc, argv);
    }
    else {
        SortDriver::run(argc,argv);
    }

    return 0;
}
