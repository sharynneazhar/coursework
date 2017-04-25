/*
============================================================================
Author        : Sharynne Azhar
Date          : 24 April 2017
Description   : Driver for Minimum Spanning Tree finder - performance testing
============================================================================
*/

#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdlib.h>
#include <string>

#include "EdgeNode/EdgeNode.h"
#include "Timer/Timer.h"
#include "MinSpanTree.h"

int main(int argc, char **argv) {
  /* Initialize and instance of a timer */
  Timer timer;
  int seed = 1;
  double kruskalResult = 0.0;
  double primResult = 0.0;

  // write results to csv file
  std::ofstream file;
  file.open("results.csv", std::ios_base::out | std::ios_base::trunc);
  file << "n,kruskal's,prim's\n\n";

  // For each values of n
  for (int n = 1000; n <= 8000; n *= 2) {
    std::cout << "\n---------------------------------";
    std::cout << "\n> N = " << n;
    std::cout << "\n---------------------------------";
    printf("\n%-5s | %-11s | %-10s", "Seed", "Kruskal's", "Prim's");
    std::cout << "\n---------------------------------";

    // For each of the five seeds
    int endSeed = seed + 5;
    for ( ; seed < endSeed; seed++) {
      // Make sure random number is the same for each seed
      srand(seed);

      // Randomly generate a graph with a total of n vertices
      int **graph = new int*[n];
      for (int i = 0; i < n; i++) {
        graph[i] = new int[n];
      }

      for (int r = 0; r < n; r++) {
        for (int c = r; c < n; c++) {
          if ((r != c )&& (rand() % 10 < 4)) {
              graph[r][c] = rand() % (4 * n) + 1;
              graph[c][r] = graph[r][c];
          } else {
            graph[r][c] = 0;
            graph[c][r] = 0;
          }
        }
      }

      // Initialize MinSpanTree
      MinSpanTree mst(graph, n);

      // Start timer and run Kruskal's
      timer.start();
      mst.runKruskal();
      kruskalResult = timer.stop();

      // Start timer and run Prims's
      timer.start();
      mst.runPrim();
      primResult = timer.stop();

      printf("\n %-4d | %-11f | %-10f", seed, kruskalResult, primResult);
      file << n << "," << kruskalResult << "," << primResult << std::endl;
    }
    std::cout << "\n---------------------------------";
    file << std::endl;
  }

  std::cout << std::endl;

  return 0;
}
