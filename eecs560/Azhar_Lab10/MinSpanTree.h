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
    DisjointSet<int> *dSet;
    EdgeNode **edgesQueue;
    int **adjMatrix;
    int dim;

    bool element(int elem, int *V);
    void sort(EdgeNode **edgesQueue, int size);

    EdgeNode *dequeue(EdgeNode **queue, int size);
    void updateQueue(EdgeNode **queue, int *V);
    bool emptyQueue(EdgeNode **queue, int size);

  public:
    MinSpanTree();
    MinSpanTree(int **matrix, int dimension);
    virtual ~MinSpanTree();
    void runKruskal();
    void runPrim();
};

#include "MinSpanTree.cpp"

#endif
