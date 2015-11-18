/**
*	@file : Sorts.hpp
*	@author : Sharynne Azhar
*	@date : 2015.10.19
*	@brief: Implementation of driver program
*/

#include <iostream>
#include <random>
#include <chrono>

#include "Sorts.h"
#include "Test.h"

void printMenu();
void printTestArray(int* arr, int size);

int main()
{
    bool done = false;
    int choice; // for menu selection
    char toPrintArray; // for printing array

    Test myTester(std::cout); // test instance

    while(!done)
    {
        printMenu();
        std::cin >> choice;

        if (choice >= 1 || choice <= 5)
        {
            int size = 0; // for array size
            int min = 0; // for array minimum
            int max = 0; // for array maximum
            double time = 0.0; // for keeping sort time

            // create an array
            std::cout << "Input a size for the random array: ";
            std::cin >> size;

            std::cout << "Input a lower bound on the range of random numbers: ";
            std::cin >> min;

            std::cout << "Input an upper bound on the range of random numbers: ";
            std::cin >> max;

            int* arr = Sorts<int>::createTestArray(size, min, max);

            // print the unsorted array
            std::cout << "Do you want to print the unsorted array? (y/n): ";
            std::cin >> toPrintArray;

            if ((toPrintArray == 'y') || (toPrintArray == 'Y'))
            {
                std::cout << "Unsorted array: \n";
                printTestArray(arr, size);
            }

            // begin sort
            switch (choice)
            {
                case 1:
                {
                    std::cout << "\nSorting with bubble sort\n";
                    time = Sorts<int>::sortTimer(Sorts<int>::bubbleSort, arr, size);
                    break;
                }
                case 2:
                {
                    std::cout << "\nSorting with insertion sort\n";
                    time = Sorts<int>::sortTimer(Sorts<int>::insertionSort, arr, size);
                    break;
                }
                case 3:
                {
                    std::cout << "\nSorting with selection sort\n";
                    time = Sorts<int>::sortTimer(Sorts<int>::selectionSort, arr, size);
                    break;
                }
                case 4:
                {
                    std::cout << "\nSorting with bogo sort\n";
                    time = Sorts<int>::sortTimer(Sorts<int>::bogoSort, arr, size);
                    break;
                }
                case 5:
                {
                    std::cout << "\nRunning tests\n";
                    myTester.runTests();
                    break;
                }
                default:
                {
                    std::cout << "\nInvalid choice!\n";
                    break;
                }
            } // end switch

            // print time took to sort
            std::cout << size << " numbers were sorted in " << time << " seconds.\n\n";

            // print the sorted array
            std::cout << "Do you want to print the sorted array? (y/n): ";
            std::cin >> toPrintArray;

            if ((toPrintArray == 'y') || (toPrintArray == 'Y'))
            {
                std::cout << "Sorted array: \n";
                printTestArray(arr, size);
            }

            delete[] arr;
            arr = nullptr;
        }
        else
        {
            std::cout << "Invalid choice!" << std::endl;
        }

        char quit;
        std::cout << "Do you want to quit? (y/n) ";
        std::cin >> quit;

        if ((quit == 'y') || (quit == 'Y'))
        {
            done = true;
        }
    } // end while

    std::cout << "Exiting...... \n\n";

    return 0;
} // end main

void printMenu()
{
	std::cout 	<< "\n\nSelect a sort:\n"
			<< "1) Bubble Sort\n"
			<< "2) Insertion Sort\n"
			<< "3) Selection Sort\n"
			<< "4) Bogo Sort (use only with very small arrays!)\n"
            << "5) Run Tests\n"
			<< "Enter choice: ";
}

void printTestArray(int* arr, int size)
{
	std::cout << "[";
	for(int i = 0; i < size - 1; i++)
    {
		std::cout << arr[i] << ", ";
	}

	std::cout << arr[size - 1] << "]\n";
}
