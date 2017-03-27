/**
*	@file   : LeftistHeapNode.h
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Holds the node object for the leftist heap
*/

#ifndef LEFTIST_HEAP_NODE_H
#define LEFTIST_HEAP_NODE_H

template<typename T>
class LeftistHeapNode {
  private:
    int m_rank;

    T m_value;

    LeftistHeapNode<T>* leftChildPtr;

    LeftistHeapNode<T>* rightChildPtr;

  public:
    LeftistHeapNode();

    LeftistHeapNode(const T& val);

    LeftistHeapNode(const T& val,
                    LeftistHeapNode<T>* leftPtr,
                    LeftistHeapNode<T>* rightPtr);

    LeftistHeapNode(const T& val,
                    const int rank,
                    LeftistHeapNode<T>* leftPtr,
                    LeftistHeapNode<T>* rightPtr);

    void setValue(const T& val);

    T getValue() const;

    void setRank(const T& rank);

    int getRank() const;

    void setLeftChildPtr(LeftistHeapNode<T>* leftPtr);

    LeftistHeapNode<T>* getLeftChildPtr() const;

    void setRightChildPtr(LeftistHeapNode<T>* rightPtr);

    LeftistHeapNode<T>* getRightChildPtr() const;

};

#include "LeftistHeapNode.cpp"

#endif
