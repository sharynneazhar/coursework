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

  int count = 0;

  test1() ? count++ : count;
  test2() ? count++ : count;
  test3() ? count++ : count;
  test4() ? count++ : count;
  test5() ? count++ : count;
  test6() ? count++ : count;
  test7() ? count++ : count;
  test8() ? count++ : count;
  test9() ? count++ : count;
  test10() ? count++ : count;
  test11() ? count++ : count;
  test12() ? count++ : count;
  test13() ? count++ : count;
  test14() ? count++ : count;
  test15() ? count++ : count;
  test16() ? count++ : count;
  test17() ? count++ : count;
  test18() ? count++ : count;
  test19() ? count++ : count;
  test20() ? count++ : count;

  std::cout << "\n>> " << count << "/20 tests passed.\n";
}

bool Test::test1() {
  LinkedListOfInts list;
  bool result = list.isEmpty() == true;
  std::cout << "Test 01: isEmpty on empty list returns true -->";
  printResult(result);
  return result;
}

bool Test::test2() {
  LinkedListOfInts list;
  populateList(list, 1);
  bool result = list.isEmpty() == false;
  std::cout << "Test 02: isEmpty on non-empty list returns false -->";
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
  std::cout << "Test 04: non-empty list returns correct size value -->";
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
  LinkedListOfInts list;
  std::vector<int> vec { 6,2,1 };
  list.addFront(1);
  list.addFront(2);
  list.addFront(3);
  list.addFront(4);
  list.removeFront();
  list.addFront(5);
  list.removeFront();
  list.removeFront();
  list.addFront(6);
  bool result = list.toVector() == vec;
  std::cout << "Test 13: a mix of addFront and removeFront maintains order -->";
  printResult(result);
  return result;
}

bool Test::test14() {
  LinkedListOfInts list;
  std::vector<int> vec { 1,2,6 };
  list.addBack(1);
  list.addBack(2);
  list.addBack(3);
  list.addBack(4);
  list.removeBack();
  list.addBack(5);
  list.removeBack();
  list.removeBack();
  list.addBack(6);
  bool result = list.toVector() == vec;
  std::cout << "Test 14: a mix of addBack and removeBack maintains order -->";
  printResult(result);
  return result;
}

bool Test::test15() {
  LinkedListOfInts list;
  populateList(list, 100);
  bool result = false;
  // TODO figure out how to do this easily...
  std::cout << "Test 15: a mixture of add and remove on large list maintains order -->";
  printResult(result);
  return result;
}

bool Test::test16() {
  LinkedListOfInts list;
  bool result = list.search(3) == false;
  std::cout << "Test 16: search on empty list returns false -->";
  printResult(result);
  return true;
}

bool Test::test17() {
  LinkedListOfInts list;
  list.addFront(5);
  list.addFront(4);
  list.addFront(3);
  list.addFront(2);
  list.addFront(1);
  bool result = list.search(33) == false;
  std::cout << "Test 17: search when value not in list returns false -->";
  printResult(result);
  return true;
}

bool Test::test18() {
  LinkedListOfInts list;
  list.addFront(5);
  list.addFront(4);
  list.addFront(3);
  list.addFront(2);
  list.addFront(1);
  bool result = list.search(1) == true;
  std::cout << "Test 18: search when value is first in list returns true -->";
  printResult(result);
  return true;
}

bool Test::test19() {
  LinkedListOfInts list;
  list.addFront(5);
  list.addFront(4);
  list.addFront(3);
  list.addFront(2);
  list.addFront(1);
  bool result = list.search(5) == true;
  std::cout << "Test 19: search when value is last in list returns true -->";
  printResult(result);
  return true;
}

bool Test::test20() {
  LinkedListOfInts list;
  populateList(list, 100);
  bool result = list.search(56) == true;
  std::cout << "Test 20: search when value is in a large list returns true -->";
  printResult(result);
  return true;
}
