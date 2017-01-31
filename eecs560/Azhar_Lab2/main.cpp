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

void generateList(std::string fileName, DoubleLinkedList<int>& list) {
  if (!fileExists(fileName)) {
    std::cout << "\nWARNING: The file " << fileName << " cannot be found. ";
    std::cout << "Please provide the name of the data file.\n>> ";
    std::cin >> fileName;
  }

  std::ifstream file;
  file.open(fileName);

  int value;
  while (file >> value) {
    list.insertValue(value);
  }
}

int main(int argc, char** argv) {
  DoubleLinkedList<int> list;

  if (argc == 2) {
    std::string fileName = argv[1];
    generateList(fileName, list);
  }
  else {
    generateList("data.txt", list);
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
        list.insertValue(inputValue);
        break;
      case 2:
        std::cout << "\nEnter a number to be deleted: ";
        std::cin >> inputValue;
        list.deleteValue(inputValue);
        break;
      case 3:
        list.reverseList();
        break;
      case 4:
        std::cout << "\nList: ";
        list.printList();
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
