/**
*	@file   : LeftistHeap.h
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Header file for the min-leftist heap data structure
*/

#ifndef LEFTIST_HEAP_H
#define LEFTIST_HEAP_H

#include <iostream>

#include "LeftistHeapNode/LeftistHeapNode.h"
#include "Queue/Queue.h"

template<typename T>
class LeftistHeap {
  private:
    LeftistHeapNode<T>* m_rootPtr;

    LeftistHeapNode<T>* merge(LeftistHeapNode<T>* leftTreePtr,
                              LeftistHeapNode<T>* rightTreePtr);

    void preorderHelper(LeftistHeapNode<T>* subTreePtr);

    void inorderHelper(LeftistHeapNode<T>* subTreePtr);

    void levelorderHelper(LeftistHeapNode<T>* subTreePtr);

    void deleteTree(LeftistHeapNode<T>* subTreePtr);

  public:
    LeftistHeap();

    virtual ~LeftistHeap();

    void insert(const T& val);

    void deleteMin();

    void preorder();

    void inorder();

    void levelorder();

};

#include "LeftistHeap.cpp"

#endif
