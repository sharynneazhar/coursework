/**
*	@file   : main.cpp
*	@author : Sharynne Azhar
*	@date   : 04-03-2017
* @brief  : Main driver for the binomial queue program
*/

#include <fstream>
#include <iostream>
#include <math.h>
#include <stdlib.h>

#include "../BinomialQueue.h"

int main(int argc, char* argv[]) {
  BinomialQueue<int> binomialQueue;
  binomialQueue.insert(3);
  binomialQueue.insert(8);
  binomialQueue.insert(6);
  binomialQueue.insert(4);
  binomialQueue.insert(2);
  binomialQueue.insert(7);
  binomialQueue.insert(9);
  binomialQueue.insert(16);
  binomialQueue.insert(1);
  binomialQueue.insert(5);
  binomialQueue.insert(10);
  binomialQueue.deleteMin();
  binomialQueue.insert(11);
  binomialQueue.insert(20);
  binomialQueue.deleteMin();
  binomialQueue.levelorder();

  return 0;
};
