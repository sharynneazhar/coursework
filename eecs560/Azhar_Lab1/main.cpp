/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 01-24-2017
*/

#include "DoubleLinkedList.h"

#include <iostream>
#include <fstream>

void printMenu() {
  std::cout << "\nPlease choose one of the following commands: ";
  std::cout << "\n1 - insert\n2 - delete\n3 - reverse\n4 - print\n5 - exit";
  std::cout << "\n\nYour choice: ";
}

bool fileExists(std::string fileName) {
  if (std::ifstream(fileName)) {
    return true;
  }
  return false;
}

void generateList(DoubleLinkedList<int>& list) {
  std::string fileName = "data.txt";
  if (!fileExists(fileName)) {
    std::cout << "\nWARNING: The file \"data.txt\" cannot be found. ";
    std::cout << "Please provide the name of the data file.\n>> ";
    std::cin >> fileName;
  }

  std::ifstream file;
  file.open(fileName);

  int value;
  while (file >> value) {
    list.insert(value);
  }
}

int main() {
  DoubleLinkedList<int> list;
  generateList(list);

  int menuOption;
  bool done = false;
  while (!done) {
    printMenu();
    std::cin >> menuOption;

    switch(menuOption) {
      case 1:
        int value;
        std::cout << "\nEnter a number to be inserted: ";
        std::cin >> value;
        list.insert(value);
        break;
      case 2:
        break;
      case 3:
        break;
      case 4:
        std::cout << "\nList: ";
        list.print();
        break;
      case 5:
        done = true;
        break;
      default:
        std::cout << "\nERROR: Invalid selection. Try again.\n";
        break;
    }
  }

  return 0;
};
