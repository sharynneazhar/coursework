/**
*	@file   : MinMaxHeap.h
*	@author : Sharynne Azhar
*	@date   : 03-07-2017
* @desc   : Header file for the min-max-heap class (bottom up approach)
*/

#ifndef MIN_MAX_HEAP_H
#define MIN_MAX_HEAP_H

#include <iostream>
#include <math.h>

#include "Queue.h"

const int MAX_HEAP_SIZE = 500;

template<typename T>
class MinMaxHeap {
  private:
    T* m_heapArr;

    int m_maxHeapSize;

    int m_numEntries;

    int getParentIndex(int index);

    int getLeftChildIndex(int index);

    int getRightChildIndex(int index);

    void swap(int index1, int index2);

    void buildHeap(const T values[]);

    void trickleDown(int index);

    void trickleDownMin(int index);

    void trickleDownMax(int index);

    void bubbleUp(int index);

    void bubbleUpMin(int index);

    void bubbleUpMax(int index);

  public:
    MinMaxHeap();

    MinMaxHeap(const T values[], const int numEntries);

    virtual ~MinMaxHeap();

    void insertItem(const T item);

    void deleteMin();

    void deleteMax();

    void levelorder();

};

#include "MinMaxHeap.cpp"

#endif
