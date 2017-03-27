/**
*	@file   : SkewHeap.h
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Header file for the min-skew heap data structure
*/

#ifndef SKEW_HEAP_H
#define SKEW_HEAP_H

#include <iostream>

#include "SkewHeapNode/SkewHeapNode.h"
#include "Queue/Queue.h"

template<typename T>
class SkewHeap{
    private:
      SkewHeapNode<T>* m_rootPtr;

      SkewHeapNode<T>* merge(SkewHeapNode<T>* leftTreePtr,
                             SkewHeapNode<T>* rightTreePtr);

      void preorderHelper(SkewHeapNode<T>* subTreePtr);

      void inorderHelper(SkewHeapNode<T>* subTreePtr);

      void levelorderHelper(SkewHeapNode<T>* subTreePtr);

      void deleteTree(SkewHeapNode<T>* subTreePtr);

    public:
      SkewHeap();

      virtual ~SkewHeap();

      void insert(const T& val);

      void deleteMin();

      void preorder();

      void inorder();

      void levelorder();


};

#include "SkewHeap.cpp"

#endif
