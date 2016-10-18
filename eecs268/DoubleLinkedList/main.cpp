/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 2015.09.21
*	@brief: Implementation file for driver program
*/

#include "Test.h"
#include "Node.h"
#include "DoubleLinkedList.h"

#include <iostream>
#include <stdexcept>

using namespace std;

void printMenu();

int main()
{
	DoubleLinkedList<int> list;
	Test myTester(cout);

	int userChoice;
	int addValue;
	int insertValueHere;
	int removeValue;

	while(true)
	{
		printMenu();
		cin >> userChoice;

		switch (userChoice)
		{
			case 1: 
				cout << "Enter a value to add: ";
				cin >> addValue;
				list.pushFront(addValue);
				cout << addValue << " added to the front of the list." << endl;
				break;
			case 2: 
				cout << "Enter a value to add: ";
				cin >> addValue;
				list.pushBack(addValue);
				cout << addValue << " added to the back of the list." << endl;
				break;
			case 3:
				cout << "Enter a value to insert: ";
				cin >> addValue;
				cout << endl;
				cout << "Enter a value to insert behind of: ";
				cin >> insertValueHere;

				try
				{
					list.insertBehind(insertValueHere, addValue);
				}
				catch (std::exception& e)
				{
					cout << e.what() << endl;
				}

				cout << addValue << " inserted behind of " << insertValueHere << endl;
				break;
			case 4:
				cout << "Enter a value to insert: ";
				cin >> addValue;
				cout << endl;
				cout << "Enter a value to insert ahead of: ";
				cin >> insertValueHere;
				
				try
				{
					list.insertAhead(insertValueHere, addValue);
				}
				catch (std::exception& e)
				{
					cout << e.what() << endl;
				}

				cout << addValue << " inserted ahead of " << insertValueHere << endl;
				break;
			case 5:
				list.removeFront();
				cout << "Front of list removed." << endl;
				break;
			case 6: 
				list.removeBack();
				cout << "Back of list removed." << endl;
				break;
			case 7:
				cout << "Enter a value to remove: ";
				cin >> removeValue;
				list.remove(removeValue);
				cout << removeValue << "removed from list." << endl;
				break;
			case 8: 
				list.printList();
				break;
			case 9:
				cout << "Exiting program." << endl;
				return (0);
			case 10:
				myTester.runTests();
				break;
		}
	}

	return (0);
}

void printMenu()
{
	std::cout << "\n\nMake choice:\n"
		<<	"1) push value onto front\n"
		<<	"2) push value onto back\n"
		<<	"3) insert behind a value\n"
		<<	"4) insert ahead of a value\n"
		<<	"5) remove front value\n"
		<<	"6) remove back value\n"
		<<	"7) remove specific value\n"
		<<	"8) print list\n"
		<<	"9) Quit\n"
		<< 	"10) Run Tests\n"
		<< 	"Your choice: ";
}
