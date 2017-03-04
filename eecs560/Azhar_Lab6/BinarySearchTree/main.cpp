/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-27-2017
*/

#include <fstream>
#include <iostream>
#include <stdlib.h>

#include "BinarySearchTree.h"

void printMenu() {
  std::cout << "\nPlease choose one of the following commands: "
            << "\n1 - insert\n2 - remove\n3 - deleteMin"
            << "\n4 - deleteMax\n5 - preorder\n6 - inorder"
            << "\n7 - levelorder\n8 - exit"
            << "\n\nYour choice: ";
}

int main(int argc, char* argv[]) {
  std::ifstream file;
  (argc == 2) ? file.open(argv[1]) : file.open("data.txt");

  if (file.fail()) {
    std::cout << "\n>> ERROR: Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  // initialze bst
  BinarySearchTree<int> bst;

  // read data from file
  int value;
  while (file >> value) {
    bst.insertItem(value);
  }

  bool done = false;
  int menuChoice, input;
  while (!done) {
    printMenu();
    std::cin >> menuChoice;
    switch (menuChoice) {
      case 1:
        std::cout << "\nEnter a number to be inserted: ";
        std::cin >> input;
        bst.insertItem(input);
        break;
      case 2:
        std::cout << "\nEnter a number to be deleted: ";
        std::cin >> input;
        bst.removeItem(input);
        break;
      case 3:
        bst.deleteMin();
        break;
      case 4:
        bst.deleteMax();
        break;
      case 5:
        bst.preorder();
        break;
      case 6:
        bst.inorder();
        break;
      case 7:
        bst.levelorder();
        break;
      case 8:
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
