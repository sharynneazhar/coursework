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
    SetNode() : rank(0), parentPtr(nullptr) {}

    SetNode(const T &k) : key(k), rank(0), parentPtr(nullptr) {}

    T getKey() const { return key; }

    void setKey(const T &k) { key = k; }

    int getRank() const { return rank; }

    void setRank(const int r) { rank = r; }

    SetNode<T> *getParentPtr() const { return parentPtr; }

    void setParentPtr(SetNode<T> *ptr) { parentPtr = ptr; }
};

#endif
