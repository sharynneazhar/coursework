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
    /* Points to the queue with the lowest order */
    BinomialQueueNode<T>* m_root;

    /* Number of elements in the binomial queue */
    int m_size;

    /* The binomial queue of binomial trees a.k.a the forest */
    BinomialQueueNode<T>* queue[MAX_NUM_TREES];

    /* Prints out binomial queue using level order traversal */
    void levelorderHelper(BinomialQueueNode<T>* ptr);

    /* Adjusts the root so that the binomial trees are always in order */
    void adjustTree();

    /* Merges a node into the binomial queue */
    void merge(BinomialQueueNode<T>* newNode);

    /* Combines two binomial trees together (q1 is the new tree) */
    BinomialQueueNode<T>* combine(BinomialQueueNode<T>* q1,
                                  BinomialQueueNode<T>* q2);

  public:
    /* Constructor */
    BinomialQueue();

    /* Destructor */
    virtual ~BinomialQueue();

    /* Returns true if queue is empty */
    bool isEmpty();

    /* Inserts a value into the binomial queue */
    void insert(const T& val);

    /* Removes the smallest value from the binomial queue */
    void deleteMin();

    /* Prints out binomial queue using level order traversal */
    void levelorder();
};

#include "BinomialQueue.cpp"

#endif
