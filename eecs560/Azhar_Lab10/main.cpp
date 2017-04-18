/*
============================================================================
Author        : Sharynne Azhar
Date          : 17 April 2017
Description   : Driver for Minimum Spanning Tree finder
============================================================================
*/

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

  int cases;   // number of graphs in the test file
  int dim;     // dimensions of the adjacency matrix

  // get number of test cases (i.e. graphs)
  file >> cases;

  // read the file
  for (int n = 0; n < cases; n++) {
    file >> dim;
    int **matrix = new int*[dim];
    for (int r = 0; r < dim; r++) {
      matrix[r] = new int[dim];
      for (int c = 0; c < dim; c++) {
        file >> matrix[r][c];
      }
    }

    MinSpanTree mst(matrix, dim);
    std::cout << "\n\nGraph " << n + 1 << ":";
    mst.runKruskal();
    mst.runPrim();
  }

  file.close();

  return 0;
}
