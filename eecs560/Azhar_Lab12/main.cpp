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

/* Placeholder value for infinity - reasonable for this lab? */
const int INFINITY = 999999;

/* Outputs the minimum cost from vertex 0 to other vertices in the graph */
void runDijkstra(int** matrix, int dim) {
  int dist[dim];      // dist[i] holds the shortest path from a vertex to u
  bool visited[dim];  // visited[i] is true if vertex u is in the shortest path tree

  // Initialize dist[] and visited[]
  for (int i = 0; i < dim; i++) {
    dist[i] = INFINITY;
    visited[i] = false;
  }

  // Vertex 0 to itself is always going to be 0, so we can set dist[0] as 0
  dist[0] = 0;

  // Find the min cost from vertex 0 to the other vertices in the graph.
  for (int i = 0; i < dim - 1; i++) {

    // Find vertex w such that dist[u] is minimized
    int w = -1;
    for (int v = 0; v < dim; v++) {
      if (!visited[v] && (w == -1 || dist[v] <= dist[w])) {
        w = v;
      }
    }

    // Add new vertex with known SP to S
    visited[w] = true;

    // Update vertices in V-S
    for (int u = 0; u < dim; u++) {
      if (matrix[w][u]) {
        if (dist[u] > dist[w] + matrix[w][u]) {
          dist[u] = dist[w] + matrix[w][u];
        }
      }
    }
  }

  // Print results
  for (int i = 0; i < dim; i++) {
    std::cout << dist[i] << " ";
  }

  std::cout << "\n\n";
}

int main(int argc, char **argv) {
  // Open file from argv[1] or default to "data.txt"
  std::ifstream file;
  file.open((argc == 2) ? argv[1] : "data.txt");

  if (file.fail()) {
    std::cout << "\n>> ERROR: Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  int numCases;   // Number of test cases (i.e. graphs)
  int numNodes;   // Number of nodes in the graph

  // Get the number of test cases from file
  file >> numCases;

  std::cout << "\nOutput:\n\n";

  // Run Dijkstra's method for all test cases
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

    // Execute Dijkstra's algorithm
    runDijkstra(matrix, numNodes);
  }

  file.close();

  return 0;
}
