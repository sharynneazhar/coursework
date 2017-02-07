/**
*	@file : HashManager.h
*	@author : Sharynne Azhar
*	@date : 02-07-2017
*/

#ifndef HASH_MANAGER_H
#define HASH_MANAGER_H

#include "QuadraticHash.h"

#include <iostream>
#include <fstream>
#include <stdlib.h>

class HashManager {
  private:
    bool done = false;
    int tableSize;

  public:
    void printSubMenu() {
      std::cout << "\nPlease choose one of the following commands: ";
      std::cout << "\n1 - insert\n2 - delete\n3 - print\n4 - exit";
      std::cout << "\n\nYour choice: ";
    }

    void init(char method, std::string filename) {
      std::ifstream file;
      file.open(filename);

      if (file.fail()) {
        std::cout << "\n>> ERROR: Unable to open file.\n";
        exit(EXIT_FAILURE);
      }

      file >> tableSize;

      if (method == 'Q') {
        std::cout << "\nQuadratic\n";

        QuadraticHash<int> hashTable(tableSize);
        int value;

        while (file >> value) {
          hashTable.insertValue(value);
        }

        runQuadraticHash(hashTable);

      } else {
        std::cout << "\nDouble Hashing\n";
      }

    }

    void runQuadraticHash(QuadraticHash<int>& hashTable) {
      int select;
      int inputValue;

      while(!done) {
        printSubMenu();
        std::cin >> select;
        switch(select) {
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

    }


};

#endif
