/**
*	@file   : main.cpp
*	@author : Sharynne Azhar
*	@date   : 04-03-2017
* @brief  : Main driver for the binomial queue program
*/

#include <fstream>
#include <iostream>
#include <math.h>
#include <stdlib.h>

#include "BinomialQueue.h"

void printMenu() {
  std::cout << "\n\nPlease choose one of the following commands: "
            << "\n1 - insert\n2 - deleteMin"
            << "\n3 - levelorder\n4 - exit"
            << "\n\nYour choice: ";
}

int main(int argc, char* argv[]) {
  std::ifstream file;
  file.open((argc == 2) ? argv[1] : "data.txt");

  if (file.fail()) {
    std::cout << "\n>> ERROR: Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  // declare the Binomial Queue
  BinomialQueue<int> binomialQueue;

  // read file
  int value;
  while (file >> value) {
    binomialQueue.insert(value);
  }

  file.close();

  bool done = false;
  int menuChoice, input;
  while (!done) {
    printMenu();
    std::cin >> menuChoice;
    switch (menuChoice) {
      case 1:
        std::cout << "\nEnter a number to be inserted: ";
        std::cin >> input;
        binomialQueue.insert(input);
        break;
      case 2:
        binomialQueue.deleteMin();
        break;
      case 3:
        binomialQueue.levelorder();
        break;
      case 4:
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
