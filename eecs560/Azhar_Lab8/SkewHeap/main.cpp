/**
*	@file   : main.cpp
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @brief  : Main driver for the min-skew heap heap program
*/

#include <fstream>
#include <iostream>
#include <math.h>
#include <stdlib.h>

#include "SkewHeap.h"

void printMenu() {
  std::cout << "\n\nPlease choose one of the following commands: "
            << "\n1 - insert\n2 - deleteMin\n3 - preorder"
            << "\n4 - inorder\n5 - levelorder\n6 - end"
            << "\n\nYour choice: ";
}

int main(int argc, char* argv[]) {
  std::ifstream file;
  file.open((argc == 2) ? argv[1] : "data.txt");

  if (file.fail()) {
    std::cout << "\n>> ERROR: Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  // declare the min-skew heap
  SkewHeap<int> skewHeap;

  // read file
  int value;
  while (file >> value) {
    skewHeap.insert(value);
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
        skewHeap.insert(input);
        break;
      case 2:
        skewHeap.deleteMin();
        break;
      case 3:
        skewHeap.preorder();
        break;
      case 4:
        skewHeap.inorder();
        break;
      case 5:
        skewHeap.levelorder();
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
