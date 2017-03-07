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

template<typename T>
class MinMaxHeap {
  private:
    T* m_heapArr;

    int m_maxHeapSize;

    int m_numEntries;

    void buildHeap();

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

    void removeItem(const T item);

    void deleteMin();

    void deleteMax();

    void levelorder() const;

};

#include "MinMaxHeap.cpp"

#endif
