/**
*	@file   : MinMaxHeap.h
*	@author : Sharynne Azhar
*	@date   : 03-07-2017
* @desc   : Header file for the min-max-heap class (bottom up approach)
*/

#ifndef MIN_HEAP_H
#define MIN_HEAP_H

#include <fstream>
#include <iostream>
#include <math.h>
#include <stdlib.h>

#include "Queue.h"

template<typename T>
class MinMaxHeap {
  private:

    void buildHeap();

  public:
    MinMaxHeap();

    MinMaxHeap(const int size);

    MinMaxHeap(const std::string fileName);

    virtual ~MinMaxHeap();

    void insertItem(const T item);

    void removeItem(const T item);

    void deleteMin();

    void deleteMax();

    void levelorder() const;

};

#include "MinMaxHeap.cpp"

#endif
