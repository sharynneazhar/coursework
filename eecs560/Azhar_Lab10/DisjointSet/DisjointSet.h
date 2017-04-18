/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 17 April 2017
 Description   : Class definition for disjoint set
 ============================================================================
*/

#ifndef DISJOINT_SET_H
#define DISJOINT_SET_H

#include <iostream>

#include "SetNode.h"

class DisjointSet {
  private:
    int numElements;
    SetNode** auxillaryArr;
    SetNode* findRoot(SetNode* setPtr);

  public:
    DisjointSet();

    DisjointSet(int* set, int size);

    virtual ~DisjointSet();

    void setUnion(const int setI, const int setJ);

    int find(int value);
};

#include "DisjointSet.cpp"

#endif
