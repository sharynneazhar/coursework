/**
*	@file : main.cpp
*	@author : Sharynne Azhar
*	@date : 02-16-2017
*/

#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdlib.h>

#include "Timer.h"
#include "OpenHash.h"
#include "ClosedHash.h"

const long MAX_VALUE = 2147483647;
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

  std::cout << "\n\nThis benchmark will run each hash algorithms 5 times.\nThis might take a while...\n\n";
  std::ofstream hashResults;
  hashResults.open("results.csv", std::ios_base::out | std::ios_base::trunc);

  for (int i = 0; i < 8; i++) {
    std::cout << "\nLoad Factor: " << loadFactors[i] << "\n";
    std::cout << std::left << std::setw(13) << std::setfill(' ') << "Open Hash";
    std::cout << std::left << std::setw(13) << std::setfill(' ') << "Quadratic";
    std::cout << std::left << std::setw(13) << std::setfill(' ') << "Double Hash";
    std::cout << std::endl;

    hashResults << "Load Factor: " << loadFactors[i] << "\n";
    hashResults << std::left << std::setw(13) << std::setfill(' ') << "Open Hash";
    hashResults << std::left << std::setw(13) << std::setfill(' ') << "Quadratic";
    hashResults << std::left << std::setw(13) << std::setfill(' ') << "Double Hash";
    hashResults << std::endl;

    int numElements = floor(DEFAULT_TABLE_SIZE * loadFactors[i]);

    for (int seedIdx = 0; seedIdx < 5; seedIdx++) {

      OpenHash<long> openHash(DEFAULT_TABLE_SIZE);
      ClosedHash<long> quadraticHash(DEFAULT_TABLE_SIZE, 60000, 'Q');
      ClosedHash<long> doubleHash(DEFAULT_TABLE_SIZE, 60000, 'D');

      srand(seedIdx);
      timer.start();
      for (int j = 0; j < numElements; j++) {
        openHash.insertValue(rand() % MAX_VALUE);
      }
      double openTime = timer.stop();
      std::cout << std::left << std::setw(13) << std::setfill(' ') << openTime;
      hashResults << openTime << ", ";
      openHashTotalTime[i] += openTime;

      srand(seedIdx);
      timer.start();
      for (int j = 0; j < numElements; j++) {
        quadraticHash.insertValue(rand() % MAX_VALUE);
      }
      double quadraticTime = timer.stop();
      std::cout << std::left << std::setw(13) << std::setfill(' ') << quadraticTime;
      hashResults << quadraticTime << ", ";
      quadraticTotalTime[i] += quadraticTime;

      srand(seedIdx);
      timer.start();
      for (int j = 0; j < numElements; j++) {
        doubleHash.insertValue(rand() % MAX_VALUE);
      }
      double doubleHashTime = timer.stop();
      std::cout << std::left << std::setw(13) << std::setfill(' ') << doubleHashTime;
      hashResults << doubleHashTime << ", ";
      doubleHashTotalTime[i] += doubleHashTime;

      std::cout << std::endl;

    }

    hashResults << std::endl;
  }

  std::cout << "\n\n\nAverages";
  hashResults << "\n\n\nAverages";

  std::cout << "\nOpen Hash" << std::left << std::setw(5) << std::setfill(' ') << ":";
  hashResults << "\nOpen Hash\n";
  for (int i = 0; i < 8; i++) {
    std::cout << std::left << std::setw(13) << std::setfill(' ') << openHashTotalTime[i] / 5;
    hashResults << openHashTotalTime[i] / 5 << ", ";
  }

  std::cout << "\nQuadratic" << std::left << std::setw(5) << std::setfill(' ') << ":";
  hashResults << "\nQuadratic\n";
  for (int i = 0; i < 8; i++) {
    std::cout << std::left << std::setw(13) << std::setfill(' ') << quadraticTotalTime[i] / 5;
    hashResults << quadraticTotalTime[i] / 5 << ", ";
  }

  std::cout << "\nDouble Hash" << std::left << std::setw(5) << std::setfill(' ') << ":";
  hashResults << "\nDouble Hash\n";
  for (int i = 0; i < 8; i++) {
    std::cout << std::left << std::setw(13) << std::setfill(' ') << doubleHashTotalTime[i] / 5;
    hashResults << doubleHashTotalTime[i] / 5 << ", ";
  }


  std::cout << "\n\n\nAll done! Results saved in \"results.txt\".\n";
  hashResults.close();

  return 0;
};
