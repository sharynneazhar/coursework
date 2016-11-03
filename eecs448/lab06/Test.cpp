/**
*	@file Test.cpp
*	@author Sharynne Azhar
*	@date 11-02-2016
*	@brief Source code for Test class
*/

#include "Test.h"

Test::Test(int size) {}



void Test::run() {
  std::cout << "run tests" << std::endl;
}

bool Test::test1() {
  LinkedListOfInts list;
  bool result = list.isEmpty();
  bool success = result == true;
  std::cout << "Test 1: Size of Empty List\n"
            << "\tExpected: true\n"
            << "\tActual: " << result
            << "\tResult: " << success
            << std::endl;
  return success;
}
