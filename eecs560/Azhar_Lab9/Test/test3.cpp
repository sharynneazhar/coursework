/**
*	@file   : main.cpp
*	@author : Sharynne Azhar
*	@date   : 04-03-2017
* @brief  : Main driver for the binomial queue program
*/

#include <iostream>
#include "../BinomialQueue.h"

int main() {
  int numItems = 1000;
  BinomialQueue<int> h;

  for (int i = 0; i < numItems; i++) {
    h.insert(i);
  }

  for (int i = 0; i < numItems; i++) {
    h.deleteMin();
  }

  if (!h.isEmpty()) {
    std::cout << "Test failed." << std::endl;
  } else {
    std::cout << "Test passed." << std::endl;
  }

  return 0;
}
