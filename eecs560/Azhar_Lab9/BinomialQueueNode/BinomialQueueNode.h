/**
*	@file   : BinomialQueueNode.h
*	@author : Sharynne Azhar
*	@date   : 04--3-2017
* @desc   : Holds the node object for the binomial queue
*/

#ifndef BINOMIAL_QUEUE_NODE_H
#define BINOMIAL_QUEUE_NODE_H

template<typename T>
class BinomialQueueNode {
  private:
    int m_order;
    T m_value;
    BinomialQueueNode<T>* firstChildPtr;
    BinomialQueueNode<T>* leftSiblingPtr;
    BinomialQueueNode<T>* rightSiblingPtr;

  public:
    BinomialQueueNode();
    BinomialQueueNode(const T& val);

    T getValue() const;
    void setValue(const T& val);

    int getOrder() const;
    void setOrder(const T& order);

    BinomialQueueNode<T>* getFirstChildPtr() const;
    void setFirstChildPtr(BinomialQueueNode<T>* childPtr);

    BinomialQueueNode<T>* getLeftSiblingPtr() const;
    void setLeftSiblingPtr(BinomialQueueNode<T>* leftPtr);

    BinomialQueueNode<T>* getRightSiblingPtr() const;
    void setRightSiblingPtr(BinomialQueueNode<T>* rightPtr);
};

#include "BinomialQueueNode.cpp"

#endif
