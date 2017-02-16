/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-16-2017
*/

#include <iostream>
#include <stdlib.h>
#include <cmath>

#include "Timer.h"
#include "OpenHash.h"

const int DEFAULT_TABLE_SIZE = 600011;

int main(int argc, char* argv[]) {

  // load factor is a  measure of how full the hash table is allowed to get
  // before its capacity is automatically increased (i.e. rehashed)
  float loadFactors[8] = { 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 };

  // keep track of results from 5 different seeds
  double openHashTimes[5];
  double quadraticTimes[5];
  double dblHashTimes[5];

  // initialize our timer
  Timer timer;

  // test the different load factors
  for (int i = 0; i < 8; i++) {
    // calculate how many elements we can insert based on load factor
    int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);

    OpenHash<long>* openHash = new OpenHash<long>(numElements);
    // ClosedHash<long>* openHash = new ClosedHash<long>(numElements, 'Q');
    // ClosedHash<long>* openHash = new ClosedHash<long>(numElements, 'D');

    timer.start();
    for (int j = 0; j < numElements; j++) {
      long num = rand() % 2147483647L; // need L at the end to preserve long
      openHash->insertValue(num);
    }

    openHashTimes[i] = timer.stop();
    std::cout << "Open Hashing: Load " << loadFactors[i]
              << ", number of elements: " << numElements
              << ", time: " << openHashTimes[i] << std::endl;

  }



  return 0;
};
