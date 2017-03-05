/**
*	@file   : MinHeap.h
*	@author : Sharynne Azhar
*	@date   : 03-02-2017
* @desc   : Header file for the min k-heap class (bottom up approach)
*/

#ifndef MIN_HEAP_H
#define MIN_HEAP_H

#include <fstream>
#include <iostream>
#include <math.h>
#include <stdlib.h>

#include "PVE.h"
#include "Queue.h"

template<typename T>
class MinHeap {
  private:
    int m_k;

    int m_maxHeapSize;

    int m_numEntries;

    T* m_heapArr;

    int getParentIndex(int childIndex) const;

    int getChildIndex(int parentIndex, int childNum) const;

    void buildHeap();

    void heapify(const int index);

    void trickleUp(const int index);

    void trickleDown(const int index);

    bool removeDuplicates(const T item);

  public:
    MinHeap();

    MinHeap(const T k, const int size);

    MinHeap(const T k, const int size, const std::string fileName);

    virtual ~MinHeap();

    void insertItem(const T item) throw (PVE);

    void removeItem(const T item);

    void deleteMin() throw (PVE);

    void deleteMax() throw (PVE);

    void levelorder() const;

};

#include "MinHeap.cpp"

#endif
