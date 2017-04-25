/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 17 April 2017
 Description   : Class definition for disjoint set node
 ============================================================================
*/

#ifndef SET_NODE_H
#define SET_NODE_H

class SetNode {
  private:
    int key;
    int rank;
    SetNode* parentPtr;

  public:
    SetNode() : rank(0), parentPtr(nullptr) {}

    SetNode(const int &k) : key(k), rank(0), parentPtr(nullptr) {}

    int getKey() const { return key; }

    void setKey(const int &k) { key = k; }

    int getRank() const { return rank; }

    void setRank(const int r) { rank = r; }

    SetNode* getParentPtr() const { return parentPtr; }

    void setParentPtr(SetNode* ptr) { parentPtr = ptr; }
};

#endif
