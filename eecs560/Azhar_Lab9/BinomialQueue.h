/**
*	@file   : BinomialQueue.h
*	@author : Sharynne Azhar
*	@date   : 04-03-2017
* @desc   : Header file for the binomial queue data structure
*/

#ifndef BINOMIAL_QUEUE_H
#define BINOMIAL_QUEUE_H

#include <iostream>

#include "BinomialQueueNode/BinomialQueueNode.h"
#include "Queue/Queue.h"

template<typename T>
class BinomialQueue {
  private:


  public:
    BinomialQueue();

    virtual ~BinomialQueue();

    void insert(const T& val);

    void deleteMin();

    void levelorder();
};

#include "BinomialQueue.cpp"

#endif
