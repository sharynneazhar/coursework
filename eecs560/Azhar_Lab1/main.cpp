/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 01-24-2017
*/

#include <iostream>

void printMenu();

int main()
{
  int menuOption;

  printMenu();
  std::cin >> menuOption;

  return 0;
};

void printMenu()
{
  std::cout << "\nPlease choose one of the following commands: ";
  std::cout << "\n1 - insert\n2 - delete\n3 - reverse\n4 - print\n5 - exit";
  std::cout << "\n\nYour choice: ";
}
