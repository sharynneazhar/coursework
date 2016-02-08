/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-08-2016
*	Purpose: main driver program
*/

#include <iostream>
#include <string>

#include "Test_LinkedList.h"

int main(int argc, char* argv[])
{
    int testSize = std::stoi(argv[1]);
    Test_LinkedList tester(testSize);
    tester.runTests();

    return 0;
}
