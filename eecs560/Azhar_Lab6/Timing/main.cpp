/**
*	@file   : main.cpp
*	@author : Sharynne Azhar
*	@date   : 03-05-2017
* @brief  : Main driver for the timing program
*/

#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdlib.h>
#include <string>

#include "Timer.h"
#include "../BinarySearchTree/BinarySearchTree.h"
#include "../MinThreeHeap/MinHeap.h"

int main() {
  Timer timer;
  double result = 0.0;

  // write results to csv file
  std::ofstream insertionFile, operationFile;
  insertionFile.open("InsertionResults.csv", std::ios_base::out | std::ios_base::trunc);
  operationFile.open("OperationResults.csv", std::ios_base::out | std::ios_base::trunc);

  int nValues[4] = { 50000, 100000, 200000, 400000 };

  double bstBuildTimes[5];
  double heapBuildTimes[5];
  double bstOperationTimes[5];
  double heapOperationTimes[5];

  std::cout << "\n\nCalculating...\n";

  for (int nIdx = 0; nIdx < 4; nIdx++) {
    std::cout << "\n-------------------------------------";
    std::cout << "\nN = " << nValues[nIdx];

    insertionFile << "N = " << nValues[nIdx] ;
    operationFile << "N = " << nValues[nIdx];

    // measure time across 5 different seeds
    for (int seedNum = 0; seedNum < 5; seedNum++) {

      // start analysis for binary search tree
      BinarySearchTree<int>* bst = new BinarySearchTree<int>();

      // insertion times
      srand(seedNum);
      timer.start();
      for (int i = 0; i < nValues[nIdx]; i++) {
        int value = rand() % (4 * nValues[nIdx]);
        bst->insertItem(value);
      }
      result = timer.stop();
      bstBuildTimes[seedNum] = result;

      // insert/delete operation times
      srand(seedNum);
      timer.start();
      for (int i = 0; i < (0.1 * nValues[nIdx]); i++) {
        double ops = (double) (rand() % 10) * 0.1;
        if (ops >= 0.5) {
          int value = rand() % (4 * nValues[nIdx]);
          bst->insertItem(value);
        } else if (ops >= 0.2) {
          int value = rand() % (4 * nValues[nIdx]);
          bst->removeItem(value);
        } else if (ops >= 0.1) {
          bst->deleteMax();
        } else {
          bst->deleteMin();
        }
      }
      result = timer.stop();
      bstOperationTimes[seedNum] = result;

      // start analysis for min 3 heap
      MinHeap<int>* heap = new MinHeap<int>(3, nValues[nIdx] * 2);

      // insertion times
      srand(seedNum);
      timer.start();
      for (int i = 0; i < nValues[nIdx]; i++) {
        int value = rand() % (4 * nValues[nIdx]);
        heap->insertItem(value);
      }
      result = timer.stop();
      heapBuildTimes[seedNum] = result;

      // insert/delee operation times
      srand(seedNum);
      timer.start();
      for (int i = 0; i < (0.1 * nValues[nIdx]); i++) {
        double ops = (double) (rand() % 10) * 0.1;
        if (ops >= 0.5) {
          int value = rand() % (4 * nValues[nIdx]);
          heap->insertItem(value);
        } else if (ops >= 0.2) {
          int value = rand() % (4 * nValues[nIdx]);
          heap->removeItem(value);
        } else if (ops >= 0.1) {
          heap->deleteMax();
        } else {
          heap->deleteMin();
        }
      }
      result = timer.stop();
      heapOperationTimes[seedNum] = result;

      delete bst;
      delete heap;
    }

    double bstBuildTimeAverage = 0;
    double bstOperationTimeAverage = 0;
    double heapBuildTimeAverage = 0;
    double heapOperationTimeAverage = 0;

    std::cout << "\n-------------------------------------";
    std::cout << "\nBuild Times";
    std::cout << "\n-------------------------------------";
    for (int i = 0; i < 5; i++) {
      std::cout << "\nSeed #" << i + 1 << ": "
                << bstBuildTimes[i] << ", "
                << heapBuildTimes[i] << "";

      insertionFile << "\nSeed #" << i + 1 << ", "
                    << bstBuildTimes[i] << ", "
                    << heapBuildTimes[i] << "";

      bstBuildTimeAverage += bstBuildTimes[i];
      heapBuildTimeAverage += heapBuildTimes[i];
    }
    std::cout << "\nAverage: " << bstBuildTimeAverage / 5.0 << ", "
              << heapBuildTimeAverage / 5.0;

    insertionFile << "\nAverage, " << bstBuildTimeAverage / 5.0 << ", "
                  << heapBuildTimeAverage / 5.0 << std::endl;

    std::cout << "\n-------------------------------------";
    std::cout << "\nOperation Times";
    std::cout << "\n-------------------------------------";
    for (int i = 0; i < 5; i++) {
      std::cout << "\nSeed #" << i + 1 << ": "
                << bstOperationTimes[i] << ", "
                << heapOperationTimes[i] << "";

      operationFile << "\nSeed #" << i + 1 << ", "
                    << bstOperationTimes[i] << ", "
                    << heapOperationTimes[i] << "";

      bstOperationTimeAverage += bstOperationTimes[i];
      heapOperationTimeAverage += heapOperationTimes[i];
    }
    std::cout << "\nAverage: " << bstOperationTimeAverage / 5.0 << ", "
              << heapOperationTimeAverage / 5.0;

    operationFile << "\nAverage, " << bstOperationTimeAverage / 5.0 << ", "
                  << heapOperationTimeAverage / 5.0 << std::endl;

    std::cout << "\n-------------------------------------\n\n";
  }

  return 0;
}
