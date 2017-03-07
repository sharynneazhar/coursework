/**
*	@file   : main.cpp
*	@author : Sharynne Azhar
*	@date   : 03-02-2017
* @brief  : Main driver for the Min 3 Heap program
*/

#include <fstream>
#include <iostream>
#include <string>

#include "MinMaxHeap.h"

void printMenu() {
  std::cout << "\nPlease choose one of the following commands: "
            << "\n1 - insert\n2 - deleteMin"
            << "\n3 - deleteMax\n4 - remove"
            << "\n5 - levelorder\n6 - exit"
            << "\n\nYour choice: ";
}

int main(int argc, char* argv[]) {
  // initialze min 3-heap from data file
  std::string fileName = (argc == 2) ? argv[1] : "data.txt";
  MinMaxHeap<int> minMaxHeap(fileName);

  bool done = false;
  int menuChoice, input;
  while (!done) {
    printMenu();
    std::cin >> menuChoice;
    switch (menuChoice) {
      case 1:
        std::cout << "\nEnter a number to be inserted: ";
        std::cin >> input;
        minMaxHeap.insertItem(input);
        break;
      case 2:
        minMaxHeap.deleteMin();
        break;
      case 3:
        minMaxHeap.deleteMax();
        break;
      case 4:
        std::cout << "\nEnter a number to be deleted: ";
        std::cin >> input;
        minMaxHeap.removeItem(input);
        break;
      case 5:
        std::cout << "\nLevelorder:\n\n";
        minMaxHeap.levelorder();
        break;
      case 6:
        std::cout << "\nBye!\n";
        done = true;
        break;
      default:
        std::cout << "\nERROR: Invalid selection. Try again.\n";
        std::cin.clear();
        std::cin.ignore(256, '\n');
        break;
    }
  }

  return 0;
};
