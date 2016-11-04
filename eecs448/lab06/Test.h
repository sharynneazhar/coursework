/**
*	@file Test.h
*	@author Sharynne Azhar
*	@date 11-02-2016
*	@brief Test class for Linked List
*/

#ifndef TEST_H
#define TEST_H

#include <iostream>
#include <vector>
#include <string>

#include "LinkedListOfInts.h"

class Test {

  private:
    void populateList(LinkedListOfInts& list, int size);
    void printVector(std::vector<int> vec);
    void printResult(bool result);


  public:
    void run(); // runs all the test

    // Tests for isEmpty()
    bool test1();  // test empty list
    bool test2();  // test non-empty list

    // Tests for size()
    bool test3();  // test size on empty list
    bool test4();  // test size on list containing multiple items

    // Tests addFront() and removeFront()
    bool test5();  // test for addFront on empty list
    bool test6();  // test for addBack on empty list

    bool test7();  // test for multiple addFronts
    bool test8();  // test for multiple addBacks

    bool test9();  // test for removeFront on empty list
    bool test10(); // test for removeBack on empty list

    bool test11(); // test for removeFront on non-empty list
    bool test12(); // test for removeBack on non-empty list

    bool test13(); // test for addFront and then removeFront
    bool test14(); // test for addBack and then removeBack

    // test for addFront and then removeBack
    // test for addBack and then removeFront

    // Tests for search()
    // test search on empty list
    // test search when value is not in list
    // test search when value is first in list
    // test search when value is last in list

    // Tests for toVector()
    // test for toVector on empty list
    // test for toVector on non-empty list

};

#endif
