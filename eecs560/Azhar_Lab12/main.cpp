/*
============================================================================
Author        : Sharynne Azhar
Date          : 25 April 2017
Description   : Driver for MST program - Dijkstra's algorithm
============================================================================
*/

#include <fstream>
#include <iostream>
#include <stdlib.h>
#include <string>

int main(int argc, char **argv) {

  std::ifstream file;
  file.open((argc == 2) ? argv[1] : "data.txt");

  if (file.fail()) {
    std::cout << "\n>> ERROR: Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  int numCases;   // Number of test cases (i.e. graphs)
  int numNodes;   // Number of nodes in the graph

  file >> numCases;

  for (int i = 0; i < numCases; i++) {
    // Get the number of nodes in this graph
    file >> numNodes;

    // Create a matrix
    int **matrix = new int*[numNodes];

    // Read data into matrix
    for (int r = 0; r < numNodes; r++) {
      matrix[r] = new int[numNodes];
      for (int c = 0; c < numNodes; c++) {
        file >> matrix[r][c];
      }
    }

    // TODO: Do Dijkstra's stuff
  }

  std::cout << std::endl;

  file.close();

  return 0;
}
