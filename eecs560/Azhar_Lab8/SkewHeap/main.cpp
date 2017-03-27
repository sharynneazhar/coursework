/**
*	@file   : main.cpp
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @brief  : Main driver for the min-leftist heap and min-skew heap program
*/

#include <fstream>
#include <iostream>
#include <math.h>
#include <stdlib.h>

#include "MinMaxHeap.h"

void printMenu() {
  std::cout << "\n\nPlease choose one of the following commands: "
            << "\n1 - insert\n2 - deleteMin\n3 - deleteMax"
            << "\n4 - levelorder\n5 - exit"
            << "\n\nYour choice: ";
}

int main(int argc, char* argv[]) {
  std::ifstream file;
  file.open((argc == 2) ? argv[1] : "data.txt");

  if (file.fail()) {
    std::cout << "\n>> ERROR: Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  // get number of entries in file
  int value, numEntries = 0;
  while (file >> value) { numEntries++; }
  file.clear();
  file.seekg(0, file.beg);

  // read in the data
  int index = 0;
  int values[numEntries];
  while (file >> value) {
    values[index] = value;
    index++;
  }

  file.close();

  // initialze min-max-heap
  MinMaxHeap<int> minMaxHeap(values, numEntries);

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
        std::cout << "\nLevelorder:";
        minMaxHeap.levelorder();
        break;
      case 5:
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
