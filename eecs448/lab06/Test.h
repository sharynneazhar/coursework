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
    void printResult(bool result);    

  public:

    void run(); // runs all the test

    // Tests for isEmpty()
    bool test1(); // test for list size zero
    bool test2(); // test for list size non-zero

    // Tests for size()
    bool test3();  // test for list size zero
    bool test4();  // test for list size non-zero

    // Tests general adding and removing
    bool test5();  // test for addFront on empty list
    bool test6();  // test for addBack on empty list

    bool test7();  // test for addFront on non-empty list
    bool test8();  // test for addBack on non-empty list

    bool test9(); // test for removeFront on empty list
    void test10(); // test for removeBack on empty list

    bool test11(); // test for removeFront on non-empty list
    bool test12(); // test for removeBack on non-empty list

    bool test13();  // test for addFront multiple times
    bool test14(); // test for addBack multiple times

    bool test15(); // test for addFront and then removeFront
    bool test16(); // test for addBack and then removeBack

    bool test17(); // test for addFront and then removeBack
    bool test18(); // test for addBack and then removeFront

    // Tests for search()
    bool test19(); // test search on empty list
    bool test20(); // test search when value is not in list
    bool test21(); // test search when value is first in list
    bool test22(); // test search when value is last in list

    // Tests for toVector()
    bool test23(); // test for toVector on empty list
    bool test24(); // test for toVector on non-empty list

};

#endif
