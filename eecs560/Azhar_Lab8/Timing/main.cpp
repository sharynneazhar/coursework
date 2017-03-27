/**
*	@file   : main.cpp
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @brief  : Main driver for the timing program
*/

#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdlib.h>
#include <string>

#include "Timer.h"
#include "../LeftistHeap/LeftistHeap.h"
#include "../SkewHeap/SkewHeap.h"

int main() {
  Timer timer;
  double result = 0.0;

  // write results to csv file
  std::ofstream insertionFile, operationFile;
  insertionFile.open("InsertionResults.csv", std::ios_base::out | std::ios_base::trunc);
  operationFile.open("OperationResults.csv", std::ios_base::out | std::ios_base::trunc);

  int nValues[4] = { 50000, 100000, 200000, 400000 };

  double leftistHeapBuildTimes[5];
  double skewHeapBuildTimes[5];
  double leftistHeapOperationTimes[5];
  double skewHeapOperationTimes[5];

  std::cout << "\n\nCalculating...\n";

  for (int nIdx = 0; nIdx < 4; nIdx++) {
    std::cout << "\n-------------------------------------";
    std::cout << "\nN = " << nValues[nIdx];

    insertionFile << "N = " << nValues[nIdx] ;
    operationFile << "N = " << nValues[nIdx];

    // measure time across 5 different seeds
    for (int seedNum = 0; seedNum < 5; seedNum++) {
      srand(seedNum);

      // start analysis for min leftist heap
      LeftistHeap<int>* leftistHeap = new LeftistHeap<int>;

      // insertion times
      timer.start();
      for (int i = 0; i < nValues[nIdx]; i++) {
        int value = rand() % (4 * nValues[nIdx]);
        leftistHeap->insert(value);
      }
      result = timer.stop();
      leftistHeapBuildTimes[seedNum] = result;

      // insert/delete operation times
      timer.start();
      for (int i = 0; i < (0.1 * nValues[nIdx]); i++) {
        double ops = (double) (rand() % 10) * 0.1;
        if (ops >= 0.5) {
          int value = rand() % (4 * nValues[nIdx]);
          leftistHeap->insert(value);
        } else {
          leftistHeap->deleteMin();
        }
      }
      result = timer.stop();
      leftistHeapOperationTimes[seedNum] = result;

      // start analysis for min 3 heap
      SkewHeap<int>* skewHeap = new SkewHeap<int>;

      // insertion times
      timer.start();
      for (int i = 0; i < nValues[nIdx]; i++) {
        int value = rand() % (4 * nValues[nIdx]);
        skewHeap->insert(value);
      }
      result = timer.stop();
      skewHeapBuildTimes[seedNum] = result;

      // insert/delee operation times
      timer.start();
      for (int i = 0; i < (0.1 * nValues[nIdx]); i++) {
        double ops = (double) (rand() % 10) * 0.1;
        if (ops >= 0.5) {
          int value = rand() % (4 * nValues[nIdx]);
          skewHeap->insert(value);
        } else {
          skewHeap->deleteMin();
        }
      }
      result = timer.stop();
      skewHeapOperationTimes[seedNum] = result;

      delete leftistHeap;
      delete skewHeap;
    }

    double leftistHeapBuildTimeAverage = 0;
    double leftistHeapOperationTimeAverage = 0;
    double skewHeapBuildTimeAverage = 0;
    double skewHeapOperationTimeAverage = 0;

    std::cout << "\n-------------------------------------";
    std::cout << "\nBuild Times";
    std::cout << "\n-------------------------------------";
    for (int i = 0; i < 5; i++) {
      std::cout << "\nSeed #" << i + 1 << ": "
                << leftistHeapBuildTimes[i] << ", "
                << skewHeapBuildTimes[i] << "";

      insertionFile << "\nSeed #" << i + 1 << ", "
                    << leftistHeapBuildTimes[i] << ", "
                    << skewHeapBuildTimes[i] << "";

      leftistHeapBuildTimeAverage += leftistHeapBuildTimes[i];
      skewHeapBuildTimeAverage += skewHeapBuildTimes[i];
    }
    std::cout << "\nAverage: " << leftistHeapBuildTimeAverage / 5.0 << ", "
              << skewHeapBuildTimeAverage / 5.0;

    insertionFile << "\nAverage, " << leftistHeapBuildTimeAverage / 5.0 << ", "
                  << skewHeapBuildTimeAverage / 5.0 << std::endl;

    std::cout << "\n-------------------------------------";
    std::cout << "\nOperation Times";
    std::cout << "\n-------------------------------------";
    for (int i = 0; i < 5; i++) {
      std::cout << "\nSeed #" << i + 1 << ": "
                << leftistHeapOperationTimes[i] << ", "
                << skewHeapOperationTimes[i] << "";

      operationFile << "\nSeed #" << i + 1 << ", "
                    << leftistHeapOperationTimes[i] << ", "
                    << skewHeapOperationTimes[i] << "";

      leftistHeapOperationTimeAverage += leftistHeapOperationTimes[i];
      skewHeapOperationTimeAverage += skewHeapOperationTimes[i];
    }
    std::cout << "\nAverage: " << leftistHeapOperationTimeAverage / 5.0 << ", "
              << skewHeapOperationTimeAverage / 5.0;

    operationFile << "\nAverage, " << leftistHeapOperationTimeAverage / 5.0 << ", "
                  << skewHeapOperationTimeAverage / 5.0 << std::endl;

    std::cout << "\n-------------------------------------\n\n";
  }

  return 0;
}
