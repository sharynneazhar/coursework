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

template<typename T>
class DisjointSet {
  private:
    int numElements;
    SetNode<T>** auxillaryArr;

    SetNode<T>* findRoot(SetNode<T>* setPtr);

  public:
    DisjointSet();

    DisjointSet(T* set, int size);

    virtual ~DisjointSet();

    void setUnion(const T setI, const T setJ);

    T find(T value);
};

#include "DisjointSet.cpp"

#endif
