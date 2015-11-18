#include "Node.h"
#include "StackInterface.h"
#include "Stack.h"
#include "Test.h"

#include <iostream>
#include <stdexcept>
#include <string>

using namespace std;

void printMenu();

int main()
{
    StackInterface<int>* lane1 = new Stack<int>();
    Test myTester(cout);

    int userChoice;

    do
    {
        printMenu();
        cin >> userChoice;

        switch (userChoice)
        {
            case 1:
                int value;
                cout << "What's going in stack 1? ";
                cin >> value;
                lane1->push(value);
                cout << value << " successfully added to stack 1" << endl;
                break;
            case 2:
                try
                {
                    cout << lane1->peek() << " is at the top of stack 1" << endl;
                }
                catch (std::exception& e)
                {
                    cout << e.what();
                }
                break;
            case 3:
                lane1->print();
                cout << endl;
                break;
            case 4:
                try
                {
                    lane1->pop();
                }
                catch (std::exception& e)
                {
                    cout << e.what();
                }
                break;
            case 5:
                cout << "Program ending..." << endl;
                break;
            case 6:
                myTester.runTests();
                break;
        }
    }while (userChoice != 5);

    delete lane1;

    return 0;
}

void printMenu()
{
	cout 	<< "\n\nSelect an action:\n"
			<< "1) Add to stack 1\n"
			<< "2) See what is at the top of stack\n"
			<< "3) Print all stack\n"
			<< "4) Pop stack\n"
			<< "5) Quit\n"
 			<< "6) Run Tests\n"
			<< "Enter choice: ";
}
