/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 17 April 2017
 Description   : Class definition for MinSpanTree
 ============================================================================
*/

#ifndef MIN_SPAN_TREE_H
#define MIN_SPAN_TREE_H

#include <iostream>

#include "EdgeNode/EdgeNode.h"
#include "DisjointSet/DisjointSet.h"

class MinSpanTree {
  private:
    DisjointSet *C;           // candidate set, C
    EdgeNode **edgesQueue;    // edges priority queue
    int **adjMatrix;          // adjacency matrix
    int dim;                  // dimensions of the adjacency matrix

    void sort(EdgeNode **queue, int size);
    bool emptyQueue(EdgeNode **queue, int size);
    int minKey(int values[], bool selected[]);

  public:
    MinSpanTree();
    MinSpanTree(int **matrix, int dimension);
    virtual ~MinSpanTree();
    EdgeNode** runKruskal();
    void runPrim();
};

#include "MinSpanTree.cpp"

#endif
