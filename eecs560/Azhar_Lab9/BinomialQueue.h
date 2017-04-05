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

// default maximum number of trees - reasonable for this lab?
const int MAX_NUM_TREES = 100;

template<typename T>
class BinomialQueue {
  private:
    BinomialQueueNode<T>* m_root;

    BinomialQueueNode<T>* bqTrees[MAX_NUM_TREES];

    void resetRootPtr();

    void insertHelper(BinomialQueueNode<T>* newNode);

    void levelorderHelper(BinomialQueueNode<T>* ptr);

    BinomialQueueNode<T>* merge(BinomialQueueNode<T>* q1,
                                BinomialQueueNode<T>* q2);

  public:
    BinomialQueue();

    virtual ~BinomialQueue();

    void insert(const T& val);

    void deleteMin();

    void levelorder();
};

#include "BinomialQueue.cpp"

#endif
