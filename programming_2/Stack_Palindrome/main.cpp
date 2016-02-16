/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-15-2016
*	@brief: Implementa-tion file for driver program-
*/

#include "Test_Stack.h"


int main(int argc, char** argv)
{
    int testSize = std::stoi(argv[1]);
    Test_Stack tester(testSize);
    tester.runTests();

    

    return 0;
}
