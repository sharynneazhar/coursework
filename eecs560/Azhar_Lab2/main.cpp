/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

#include "OpenHasher.h"

#include <iostream>
#include <fstream>
#include <stdexcept>

void printMenu() {
  std::cout << "\nPlease choose one of the following commands: ";
  std::cout << "\n1 - insert\n2 - delete\n3 - print\n4 - exit";
  std::cout << "\n\nYour choice: ";
}

bool isPrime(int num) {
  int count = 0;
  for (int i = 1; i <= num; i++) {
    if (num % i == 0)
      count++;
  }
  return count == 2;
}

OpenHasher<int> generateHashTable(std::string fileName) {
  std::ifstream file;
  file.open(fileName);

  if (file.fail()) {
    std::cout << "\nWARNING: Unable to open file " << fileName;
    std::cout << ". Please provide the name of the data file.\n>> ";
    std::cin >> fileName;
  }

  int value, size;
  file >> size;

  OpenHasher<int> hashTable(size);
  if (!isPrime(size)) {
    std::string msg = "\nERROR: Hash table size \""
                    + std::to_string(size)
                    + "\" should be a prime number\n";
    throw std::runtime_error(msg);
  }

  while (file >> value) {
    hashTable.insertValue(value);
  }

  return hashTable;
}

int main(int argc, char** argv) {
  OpenHasher<int> hashTable;

  try {
    if (argc == 2) {
      std::string fileName = argv[1];
      hashTable = generateHashTable(fileName);
    }
    else {
      hashTable = generateHashTable("data.txt");
    }
  } catch (const std::exception& e) {
    std::cout << e.what();
    return 0;
  }


  bool done = false;
  int menuOption;
  int inputValue;

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
