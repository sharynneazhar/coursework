/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-16-2017
*/

#include <cmath>
#include <iomanip>
#include <iostream>
#include <stdlib.h>

#include "Timer.h"
#include "OpenHash.h"
#include "ClosedHash.h"

const int DEFAULT_TABLE_SIZE = 600011;

void prettyPrint(std::string hashMethod, float loadFactors[8],
  double seedTimes[5][8], double averageTimes[8]) {
  std::cout << std::endl;
  std::cout << std::left << std::setw(118) << std::setfill('-') << "-";
  std::cout << "\n" << hashMethod << "\n";
  std::cout << std::left << std::setw(118) << std::setfill('-') << "-";

  std::cout << std::left << std::setw(18) << std::setfill(' ') << "\nLoad Factor:";
  for (int i = 0; i < 8; i++) {
    std::cout << std::left << std::setw(13) << std::setfill(' ') << loadFactors[i];
  }
  std::cout << std::endl;
  std::cout << std::left << std::setw(118) << std::setfill('-') << "-";

  for (int i = 1; i <= 5; i++) {
    std::cout << std::left << std::setw(18) << std::setfill(' ') << "\nSeed #";
    for (int j = 0; j < 8; j++) {
      std::cout << std::left << std::setw(13) << std::setfill(' ') << seedTimes[i][j];
    }
  }

  std::cout << std::endl;
  std::cout << std::left << std::setw(118) << std::setfill('-') << "-";
  std::cout << std::left << std::setw(18) << std::setfill(' ') << "\nAverage Time:";
  for (int i = 0; i < 8; i++) {
    std::cout << std::left << std::setw(13) << std::setfill(' ') << averageTimes[i] / 5;
  }

  std::cout << std::endl;
  std::cout << std::left << std::setw(118) << std::setfill('-') << "-" << std::endl;
}

int main(int argc, char* argv[]) {

  // load factor is a  measure of how full the hash table is allowed to get
  // before its capacity is automatically increased (i.e. rehashed)
  float loadFactors[8] = { 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 };

  // keep track of results from different load factors
  double openHashLoadFactorTimes[8];
  double quadraticLoadFactorTimes[8];
  double doubleHashLoadFactorTimes[8];

  // keep track result from 5 different seeds
  double openHashSeedTimes[5][8];
  double quadraticSeedTimes[5][8];
  double doubleHashSeedTimes[5][8];

  // keep track average result from 5 different seeds
  double openHashAverageTimes[8];
  double quadraticAverageTimes[8];
  double doubleHashAverageTimes[8];

  // initialize our timer
  Timer timer;

  std::cout << "\n\n>> Calculating open hashing method...\n\n";
  for (int seedIdx = 1; seedIdx <= 5; seedIdx++) {
    srand(seedIdx);
    timer.start();
    for (int i = 0; i < 8; i++) {
      // calculate how many elements we can insert based on load factor
      int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);
      OpenHash<long>* openHash = new OpenHash<long>(numElements);
      for (int j = 0; j < numElements; j++) {
        long num = rand() % 2147483647L; // need L at the end to preserve long
        openHash->insertValue(num);
      }
      double _time = timer.stop();
      openHashSeedTimes[seedIdx][i] += _time;
      openHashAverageTimes[i] += openHashSeedTimes[seedIdx][i];
      delete openHash;
    }
  }

  prettyPrint("Open Hashing", loadFactors, openHashSeedTimes, openHashAverageTimes);

  // std::cout << "\n>> Calculating quadratic probing method...\n\n";
  // timer.start();
  // for (int i = 0; i < 8; i++) {
  //   int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);
  //   ClosedHash<long>* quadraticHash = new ClosedHash<long>(numElements, 'Q');
  //   for (int j = 0; j < numElements; j++) {
  //     long num = rand() % 2147483647L; // need L at the end to preserve long
  //     quadraticHash->insertValue(num);
  //   }
  //   quadraticLoadFactorTimes[i] = timer.stop();
  //   delete quadraticHash;
  // }
  // prettyPrint("Quadratic Probing", loadFactors, quadraticLoadFactorTimes, 8);
  //
  // std::cout << "\n>> Calculating double hashing method...\n\n";
  // timer.start();
  // for (int i = 0; i < 8; i++) {
  //   int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);
  //   ClosedHash<long>* doubleHash = new ClosedHash<long>(numElements, 'D');
  //   for (int j = 0; j < numElements; j++) {
  //     long num = rand() % 2147483647L; // need L at the end to preserve long
  //     doubleHash->insertValue(num);
  //   }
  //   doubleHashLoadFactorTimes[i] = timer.stop();
  //   delete doubleHash;
  // }
  // prettyPrint("Double Hashing", loadFactors, doubleHashLoadFactorTimes, 8);


  return 0;
};
