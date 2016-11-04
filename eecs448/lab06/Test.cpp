/**
*	@file Test.cpp
*	@author Sharynne Azhar
*	@date 11-02-2016
*	@brief Source code for Test class
*/

#include "Test.h"

void Test::populateList(LinkedListOfInts& list, int size) {
  // bug in addBack so using addFront instead and reverse loop
  for (int i = size - 1; i >= 0; i--) {
    list.addFront(i);
  }
}

void Test::printVector(std::vector<int> vec) {
  for (std::size_t i = 0; i < vec.size(); i++) {
    std::cout << vec.at(i) << " ";
  }
  std::cout << std::endl;
}

void Test::printResult(bool result) {
  if (result) {
    std::cout << "\033[1;32m PASSED \033[0m" << std::endl;
  } else {
    std::cout << "\033[1;31m FAILED \033[0m" << std::endl;
  }
}

void Test::run() {
  std::cout << "\n=================\n";
  std::cout << "  RUNNING TESTS   \n";
  std::cout << "=================\n\n";

  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
  test7();
  test8();
  test9();
  test10();
  test11();
  test12();
  test13();
  test14();
}

bool Test::test1() {
  LinkedListOfInts list;
  bool result = list.isEmpty() == true;
  std::cout << "Test 01: empty list returns true -->";
  printResult(result);
  return result;
}

bool Test::test2() {
  LinkedListOfInts list;
  populateList(list, 1);
  bool result = list.isEmpty() == false;
  std::cout << "Test 02: non-empty list returns false -->";
  printResult(result);
  return result;
}

bool Test::test3() {
  LinkedListOfInts list;
  bool result = list.size() == 0;
  std::cout << "Test 03: empty list returns size 0 -->";
  printResult(result);
  return result;
}

bool Test::test4() {
  LinkedListOfInts list;
  populateList(list, 3);
  bool result = list.size() == 3;
  std::cout << "Test 04: non-empty list returns correct value -->";
  printResult(result);
  return result;
}

bool Test::test5() {
  LinkedListOfInts list;
  list.addFront(1);
  bool result = (list.size() == 1) && (list.toVector().front() == 1);
  std::cout << "Test 05: addFront on empty list returns correct size and value -->";
  printResult(result);
  return result;
}

bool Test::test6() {
  LinkedListOfInts list;
  list.addBack(1);
  bool result = (list.size() == 1) && (list.toVector().front() == 1);
  std::cout << "Test 06: addBack on empty list returns correct size and value -->";
  printResult(result);
  return result;
}

bool Test::test7() {
  LinkedListOfInts list;
  std::vector<int> vec { 3,2,1 };
  list.addFront(1);
  list.addFront(2);
  list.addFront(3);
  bool result = (list.size() == 3) && (list.toVector() == vec);
  std::cout << "Test 07: multiple addFronts returns correct size and list order -->";
  printResult(result);
  return result;
}

bool Test::test8() {
  LinkedListOfInts list;
  std::vector<int> vec { 1,2,3 };
  list.addBack(1);
  list.addBack(2);
  list.addBack(3);
  bool result = (list.size() == 3) && (list.toVector() == vec);
  std::cout << "Test 08: multiple addBacks returns correct size and list order -->";
  printResult(result);
  return result;
}

bool Test::test9() {
  LinkedListOfInts list;
  bool result = list.removeFront() == false;
  std::cout << "Test 09: removeFront on empty list returns false -->";
  printResult(result);
  return result;
}

bool Test::test10() {
  LinkedListOfInts list;
  bool result = list.removeBack() == false;
  std::cout << "Test 10: removeBack on empty list returns false -->";
  printResult(result);
  return result;
}

bool Test::test11() {
  LinkedListOfInts list;
  std::vector<int> vec { 3,2,1 };
  list.addFront(1);
  list.addFront(2);
  list.addFront(3);
  list.addFront(4);
  bool isRemoved = list.removeFront();
  bool result = isRemoved && (list.toVector() == vec);
  std::cout << "Test 11: removeFront on non-empty list maintains order and returns true -->";
  printResult(result);
  return result;
}

bool Test::test12() {
  LinkedListOfInts list;
  std::vector<int> vec { 3,2,1 };
  list.addFront(1);
  list.addFront(2);
  list.addFront(3);
  list.addFront(4);
  bool isRemoved = list.removeBack();
  bool result = isRemoved && (list.toVector() == vec);
  std::cout << "Test 12: removeBack on non-empty list maintains order and returns true -->";
  printResult(result);
  return result;
}

bool Test::test13() {

}

bool Test::test14() {
  
}


// std::cout << list.size() << " -- ";
// printVector(list.toVector());
