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

void prettyPrint(std::string hashMethod, float loadFactors[], double loadTimes[], int length) {
  std::cout << std::left << std::setw(30) << std::setfill('-') << "-";
  std::cout << "\n" << hashMethod << "\n";
  std::cout << std::left << std::setw(30) << std::setfill('-') << "-";

  std::cout << std::endl;
  std::cout << std::left << std::setw(15) << std::setfill(' ') << "Load Factor";
  std::cout << std::left << std::setw(5) << std::setfill(' ') << "Time\n";
  std::cout << std::left << std::setw(30) << std::setfill('-') << "-" << std::endl;

  for (int i = 0; i < length; i++) {
    std::cout << std::left << std::setw(15) << std::setfill(' ') << loadFactors[i];
    std::cout << std::left << std::setw(15) << std::setfill(' ') << loadTimes[i];
    std::cout << std::endl;
  }

  std::cout << std::left << std::setw(30) << std::setfill('-') << "-" << std::endl;
}

int main(int argc, char* argv[]) {

  // load factor is a  measure of how full the hash table is allowed to get
  // before its capacity is automatically increased (i.e. rehashed)
  float loadFactors[8] = { 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 };

  // keep track of results from different load factors
  double openHashLoadFactorTimes[8];
  double quadraticLoadFactorTimes[8];
  double doubleHashLoadFactorTimes[8];

  // initialize our timer
  Timer timer;

  // Run
  std::cout << "\n\n>> Calculating open hashing method...\n\n";
  timer.start();
  for (int i = 0; i < 8; i++) {
    // calculate how many elements we can insert based on load factor
    int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);
    OpenHash<long>* openHash = new OpenHash<long>(numElements);
    for (int j = 0; j < numElements; j++) {
      long num = rand() % 2147483647L; // need L at the end to preserve long
      openHash->insertValue(num);
    }
    openHashLoadFactorTimes[i] = timer.stop();
    delete openHash;
  }
  prettyPrint("Open Hashing", loadFactors, openHashLoadFactorTimes, 8);

  std::cout << "\n>> Calculating quadratic probing method...\n\n";
  timer.start();
  for (int i = 0; i < 8; i++) {
    int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);
    ClosedHash<long>* quadraticHash = new ClosedHash<long>(numElements, 'Q');
    for (int j = 0; j < numElements; j++) {
      long num = rand() % 2147483647L; // need L at the end to preserve long
      quadraticHash->insertValue(num);
    }
    quadraticLoadFactorTimes[i] = timer.stop();
    delete quadraticHash;
  }
  prettyPrint("Quadratic Probing", loadFactors, quadraticLoadFactorTimes, 8);

  std::cout << "\n>> Calculating double hashing method...\n\n";
  timer.start();
  for (int i = 0; i < 8; i++) {
    int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);
    ClosedHash<long>* doubleHash = new ClosedHash<long>(numElements, 'D');
    for (int j = 0; j < numElements; j++) {
      long num = rand() % 2147483647L; // need L at the end to preserve long
      doubleHash->insertValue(num);
    }
    doubleHashLoadFactorTimes[i] = timer.stop();
    delete doubleHash;
  }
  prettyPrint("Double Hashing", loadFactors, doubleHashLoadFactorTimes, 8);


  return 0;
};
