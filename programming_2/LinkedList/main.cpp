/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-08-2016
*	Purpose: main driver program
*/

#include <iostream>
#include <string>

#include "Test_LinkedList.h"

int main(int argc, char** argv)
{
    int testSize = 20; // std::stoi(argv[1]);
    Test_LinkedList tester(testSize);
    tester.runTests();

    std::cout << "\n\n";
    LinkedList list;
    list.addFront(1);
    list.addBack(2);
    list.addBack(2);
    list.addBack(2);
    list.addBack(2);
    list.addBack(3);
    list.addBack(4);

    list.printList();

    std::cout << "\n\nOccurs " << list.count(1) << " times\n";
    std::cout << "Occurs " << list.count(2) << " times\n";
    std::cout << "Occurs " << list.count(4) << " times\n";
    std::cout << "Occurs " << list.count(6) << " times\n\n";

    // list.insertAhead(7, 1);
    // list.insertAhead(8, 3);
    // list.insertAhead(5, 4);

    list.removeDuplicates();
    std::cout << "\n\n";

    list.printList();

    return 0;
}
