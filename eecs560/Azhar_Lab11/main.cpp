/*
============================================================================
Author        : Sharynne Azhar
Date          : 17 April 2017
Description   : Driver for Minimum Spanning Tree finder
============================================================================
*/

// 1:5 for n = 1000
// 6:10 for n = 2000
// 11:15 for n = 4000
// 16:20 for n = 8000

#include <fstream>
#include <iostream>
#include <stdlib.h>
#include <string>

#include "MinSpanTree.h"

int main(int argc, char **argv) {
  std::ifstream file;
  file.open((argc == 2) ? argv[1] : "data.txt");

  if (file.fail()) {
    std::cout << "\n>> ERROR: Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  int numGraphs;   // number of graphs in the test file
  int dim;         // dimensions of the adjacency matrix

  // get number of test cases (i.e. graphs)
  file >> numGraphs;

  // read the file
  for (int n = 0; n < numGraphs; n++) {
    // read the dimensions of the matrix
    file >> dim;

    // create a matrix
    int **matrix = new int*[dim];

    // build the matrix
    for (int r = 0; r < dim; r++) {
      matrix[r] = new int[dim];
      for (int c = 0; c < dim; c++) {
        file >> matrix[r][c];
      }
    }

    // initialize the tree
    MinSpanTree mst(matrix, dim);

    // run the algorithms
    std::cout << "\n\nGraph " << n + 1 << ":";
    mst.runKruskal();
    mst.runPrim();
  }

  std::cout << std::endl;

  file.close();

  return 0;
}
