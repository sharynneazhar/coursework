/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

#include "HashTable.h"

#include <iostream>
#include <fstream>
#include <stdlib.h>

void printMainMenu() {
  std::cout << "\nPlease choose one of the following hash methods: ";
  std::cout << "\n1 - quadratic\n2 - double hashing\n3 - exit";
  std::cout << "\n\nYour choice: ";
}

void printSubMenu() {
  std::cout << "\nPlease choose one of the following commands: ";
  std::cout << "\n1 - insert\n2 - delete\n3 - print\n4 - exit";
  std::cout << "\n\nYour choice: ";
}

void run(std::string fileName, char hashMethod) {
  std::ifstream file;
  file.open(fileName);

  if (file.fail()) {
    std::cout << "\n>> ERROR: Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  int tableSize, value;
  file >> tableSize;
  HashTable<int> hashTable(tableSize, hashMethod);
  while (file >> value) {
    hashTable.insertValue(value);
  }

  file.close();

  bool done = false;
  int select, input;
  while(!done) {
    printSubMenu();
    std::cin >> select;
    switch(select) {
      case 1:
        std::cout << "\nEnter a number to be inserted: ";
        std::cin >> value;
        hashTable.insertValue(input);
        break;
      case 2:
        std::cout << "\nEnter a number to be deleted: ";
        std::cin >> value;
        hashTable.deleteValue(input);
        break;
      case 3:
        hashTable.printList();
        break;
      case 4:
        done = true;
        break;
      default:
        std::cout << "\nERROR: Invalid selection. Try again.\n";
        std::cin.clear(); // clear the input stream
        std::cin.ignore(256, '\n'); // ignore any remaining buffers
        break;
    }
  }
}

int main(int argc, char* argv[]) {
  std::string fileName = (argc == 2) ? argv[1] : "data.txt";

  int menuOption;
  while (true) {
    printMainMenu();
    std::cin >> menuOption;
    switch (menuOption) {
      case 1:
        run(fileName, 'Q');
        break;
      case 2:
        run(fileName, 'D');
        break;
      case 3:
        exit(EXIT_SUCCESS);
        break;
      default:
        std::cout << "\nERROR: Invalid selection. Try again.\n";
        std::cin.clear(); // clear the input stream
        std::cin.ignore(256, '\n'); // ignore any remaining buffers
        break;
    }
  }

  return 0;
};
