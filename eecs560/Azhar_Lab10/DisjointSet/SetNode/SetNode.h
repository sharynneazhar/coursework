/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 17 April 2017
 Description   : Class definition for disjoint set node
 ============================================================================
*/

#ifndef SET_NODE_H
#define SET_NODE_H

template<typename T>
class SetNode {
  private:
    T key;
    int rank;
    SetNode<T> *parentPtr;

  public:
    SetNode();
    SetNode(const T &k);

    T getKey() const;
    void setKey(const T &k);

    int getRank() const;
    void setRank(const int r);

    SetNode<T> *getParentPtr() const;
    void setParentPtr(SetNode<T> *nodePtr);
};

#include "SetNode.cpp"

#endif
