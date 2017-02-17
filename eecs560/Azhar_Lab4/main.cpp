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

// adds horizontal line
void hr() { std::cout << std::left << std::setw(115) << std::setfill('-') << "-"; }

int main(int argc, char* argv[]) {

  // load factor is a  measure of how full the hash table is allowed to get
  // before its capacity is automatically increased (i.e. rehashed)
  float loadFactors[8] = { 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 };

  // keep track average result from 5 different seeds
  double openHashTotalTime[8];
  double quadraticTotalTime[8];
  double doubleHashTotalTime[8];

  // initialize our timer
  Timer timer;

  // run 5 different trials
  std::cout << "\n\nOpen Hashing\n"; hr();
  for (int seedIdx = 1; seedIdx <= 5; seedIdx++) {
    std::cout << "\nSeed #" << seedIdx << std::left << std::setw(5) << std::setfill(' ') << ":";
    srand(seedIdx);
    timer.start();
    for (int i = 0; i < 8; i++) {
      int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);
      OpenHash<long>* openHash = new OpenHash<long>(numElements);
      for (int j = 0; j < numElements; j++) {
        openHash->insertValue(rand() % 2147483647L);
      }
      double _time = timer.stop();
      std::cout << std::left << std::setw(13) << std::setfill(' ') << _time;
      openHashTotalTime[i] += _time;
      delete openHash;
    }
  }

  std::cout << std::endl; hr();
  std::cout << "\nAverage" << std::left << std::setw(5) << std::setfill(' ') << ":";
  for (int i = 0; i < 8; i++) {
    std::cout << std::left << std::setw(13) << std::setfill(' ') << openHashTotalTime[i] / 5;
  }

  std::cout << "\n\n\nQuadratic Probing\n"; hr();
  for (int seedIdx = 1; seedIdx <= 5; seedIdx++) {
    std::cout << "\nSeed #" << seedIdx << std::left << std::setw(5) << std::setfill(' ') << ":";
    srand(seedIdx);
    timer.start();
    for (int i = 0; i < 8; i++) {
      int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);
      ClosedHash<long>* quadraticHash = new ClosedHash<long>(numElements, 'Q');
      for (int j = 0; j < numElements; j++) {
        quadraticHash->insertValue(rand() % 2147483647L);
      }
      double _time = timer.stop();
      std::cout << std::left << std::setw(13) << std::setfill(' ') << _time;
      quadraticTotalTime[i] += _time;
      delete quadraticHash;
    }
  }

  std::cout << std::endl; hr();
  std::cout << "\nAverage" << std::left << std::setw(5) << std::setfill(' ') << ":";
  for (int i = 0; i < 8; i++) {
    std::cout << std::left << std::setw(13) << std::setfill(' ') << quadraticTotalTime[i] / 5;
  }

  std::cout << "\n\n\nDouble Hashing\n"; hr();
  for (int seedIdx = 1; seedIdx <= 5; seedIdx++) {
    std::cout << "\nSeed #" << seedIdx << std::left << std::setw(5) << std::setfill(' ') << ":";
    srand(seedIdx);
    timer.start();
    for (int i = 0; i < 8; i++) {
      int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);
      ClosedHash<long>* doubleHash = new ClosedHash<long>(numElements, 'D');
      for (int j = 0; j < numElements; j++) {
        doubleHash->insertValue(rand() % 2147483647L);
      }
      double _time = timer.stop();
      std::cout << std::left << std::setw(13) << std::setfill(' ') << _time;
      doubleHashTotalTime[i] += _time;
      delete doubleHash;
    }
  }

  std::cout << std::endl; hr();
  std::cout << "\nAverage" << std::left << std::setw(5) << std::setfill(' ') << ":";
  for (int i = 0; i < 8; i++) {
    std::cout << std::left << std::setw(13) << std::setfill(' ') << doubleHashTotalTime[i] / 5;
  }


  std::cout << "\n\nAll done!\n\n";

  return 0;
};
