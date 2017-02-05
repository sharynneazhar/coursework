/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

#include "HashTable.h"

#include <iostream>
#include <fstream>
#include <stdlib.h>

void printMenu() {
  std::cout << "\nPlease choose one of the following commands: ";
  std::cout << "\n1 - insert\n2 - delete\n3 - print\n4 - exit";
  std::cout << "\n\nYour choice: ";
}

int main(int argc, char* argv[]) {
  std::ifstream file;
  (argc == 2) ? file.open(argv[1]) : file.open("data.txt");

  if (file.fail()) {
    std::cout << "\n>> ERROR: Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  // get table size 
  int size;
  file >> size;

  // generate hash table with size
  HashTable<int> hashTable(size);

  // read in the red of the values
  int value;
  while (file >> value) {
    hashTable.insertValue(value);
  }

  bool done = false;
  int menuOption, inputValue;
  while (!done) {
    printMenu();
    std::cin >> menuOption;
    switch(menuOption) {
      case 1:
        std::cout << "\nEnter a number to be inserted: ";
        std::cin >> inputValue;
        hashTable.insertValue(inputValue);
        break;
      case 2:
        std::cout << "\nEnter a number to be deleted: ";
        std::cin >> inputValue;
        hashTable.deleteValue(inputValue);
        break;
      case 3:
        hashTable.printList();
        break;
      case 4:
        done = true;
        break;
      default:
        std::cout << "\nERROR: Invalid selection. Try again.\n";
        break;
    }
  }

  return 0;
};
