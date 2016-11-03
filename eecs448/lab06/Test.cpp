/**
*	@file Test.cpp
*	@author Sharynne Azhar
*	@date 11-02-2016
*	@brief Source code for Test class
*/

#include "Test.h"

void Test::run() {
  std::cout << "\n\n=================\n";
  std::cout << "  RUNNING TESTS   \n";
  std::cout << "=================\n\n";

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
}

void Test::printResult(bool result) {
  if (result) {
    std::cout << "PASSED" << std::endl;
  } else {
    std::cout << "FAILED" << std::endl;
  }
}

bool Test::test1() {
  LinkedListOfInts list;
  bool result = list.isEmpty() == true;
  std::cout << "Test 01: empty list should return true --> ";
  printResult(result);
  return result;
}

bool Test::test2() {
  LinkedListOfInts list;
  list.addFront(1);
  bool result = list.isEmpty() == false;
  std::cout << "Test 02: non-empty list should return false --> ";
  printResult(result);
  return result;
}

bool Test::test3() {
  LinkedListOfInts list;
  bool result = list.size() == 0;
  std::cout << "Test 03: size returns 0 on empty list --> ";
  printResult(result);
  return result;
}

bool Test::test4() {
  LinkedListOfInts list;
  list.addFront(1);
  bool result = list.size() != 0;
  std::cout << "Test 04: size returns non-zero on non-empty list --> ";
  printResult(result);
  return result;
}

bool Test::test5() {
  LinkedListOfInts list;
  list.addFront(1);
  bool result = list.size() == 1;
  std::cout << "Test 05: size returns 1 after adding to front of empty list --> ";
  printResult(result);
  return result;
}

bool Test::test6() {
  LinkedListOfInts list;
  list.addBack(1);
  bool result = list.size() == 1;
  std::cout << "Test 06: size returns 1 after adding to back of empty list --> ";
  printResult(result);
  return result;
}
