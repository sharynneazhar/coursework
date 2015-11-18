/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 2015.09.17
*	@brief: Implementation file of the driver program. 
*/

#include "Node.h"
#include "LinkedList.h"
#include "Test.h"

#include <iostream>

using namespace std;

void printMenu();

int main() 
{
	LinkedList list;
	Test myTest(cout);

	int userChoice;
	int value;

	do 
	{
		printMenu();
	    cin >> userChoice;

	    switch (userChoice)
	    {
	    	case 1: 
	    		cout << "Enter a value to add: ";
	    		cin >> value;
	    		cout << "Adding " << value << " to the list." << endl;
	    		list.addFront(value);
	    		break;
	    	case 2: 
	    		cout << "Enter a value to add: ";
	    		cin >> value;
	    		cout << "Adding " << value << " to the list." << endl;
	    		list.addBack(value);
	    		break;
	    	case 3:
	    		cout << "Removing first item on the list..." << endl;
	   			list.removeFront();
	    		break;
	    	case 4:
	    		cout << "Removing last item on the list..." << endl;
	   			list.removeBack();
	    		break;
	    	case 5:
	    		cout << "Printing list..." << endl;
	    		list.printList();
	    		break;
	    	case 6:
	    		cout << "Enter a value to search for: ";
	    		cin >> value;
	    		cout << "Searching for " << value << "..." << endl;
	    		list.search(value);
	    		break;
	    	case 7:
	    		cout << "Exiting..." << endl;
	    		return 0;
	    	case 8:
	    		myTest.runTests();
	    		break;
	    	default:
	    		cout << "Invalid input!" << endl;
	    		break;
	    }
	}while(userChoice != 7);
   
	return (0);
}

void printMenu()
{
	cout << "\n\nSelect from the following menu:\n"
			<< "1) Add to the front of the list\n"
			<< "2) Add to the end of the list\n"
			<< "3) Remove from front of the list\n"
			<< "4) Remove from back of the list\n"
			<< "5) Print the list\n"
			<< "6) Search for value\n"
			<< "7) Exit\n"
			<< "8) Run tests\n"
			<< "Enter your choice: ";
}
