/**
*	@file   : Queue.h
*	@author : Sharynne Azhar
*	@date   : 02-28-2017
* @desc   : Header file of Queue class for level order traversing
*/

#ifndef QUEUE_H
#define QUEUE_H

#include "Node.h"
#include "PVE.h"

template<typename T>
class Queue {

  private:
    int m_size;

    Node<T>* m_front;

    Node<T>* m_back;

  public:
    Queue();

    virtual ~Queue();

    bool isEmpty() const;

    void enqueue(const T& item);

    void dequeue() throw (PVE);

    T peek() const throw (PVE);

    T getNext() const throw (PVE);

    void print() const;

};

#include "Queue.cpp"

#endif
