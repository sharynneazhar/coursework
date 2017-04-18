/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 17 April 2017
 Description   : Class definition for EdgeNode
 ============================================================================
*/

#ifndef EDGE_NODE_H
#define EDGE_NODE_H

class EdgeNode {
  private:
    int w;
    int a;
    int b;

  public:
    EdgeNode();
    EdgeNode(const int weight, const int vert1, const int vert2);

    int getWeight() const;
    void setWeight(const int weight);

    int getV1() const;
    void setV1(const int vert1);

    int getV2() const;
    void setV2(const int vert2);
};

#include "EdgeNode.cpp"

#endif
