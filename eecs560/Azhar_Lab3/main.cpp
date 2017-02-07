/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

#include "HashManager.h"

#include <iostream>
#include <stdlib.h>

void printMainMenu() {
  std::cout << "\nPlease choose one of the following hash methods: ";
  std::cout << "\n1 - quadratic\n2 - double hashing\n3 - exit";
  std::cout << "\n\nYour choice: ";
}

int main(int argc, char* argv[]) {
  HashManager manager;
  std::string fileName = (argc == 2) ? argv[1] : "data.txt";

  int menuOption;
  while (true) {
    printMainMenu();
    std::cin >> menuOption;
    switch (menuOption) {
      case 1:
        manager.init('Q', fileName);
        break;
      case 2:
        manager.init('D', fileName);
        break;
      case 3:
        std::cout << "\nExiting...\n\n";
        exit(EXIT_SUCCESS);
        break;
      default:
        std::cout << "\nERROR: Invalid selection. Try again.\n";
        break;
    }
  }

  return 0;
};
