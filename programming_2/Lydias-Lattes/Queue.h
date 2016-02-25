/**
*	@file : Queue.h
*	@author : Sharynne Azhar
*	@date : 02-22-2016
*	@brief: Header file for the queue class used to store nodes in a queue
*/

#ifndef QUEUE_H
#define QUEUE_H

#include "QueueInterface.h"

template <typename T>
class Queue : public QueueInterface<T>
{
    private:
        int m_size;
        Node<T>* m_front;
        Node<T>* m_back;

    public:

        /*
        * @post initalizes an instance of the Queue class
        */
        Queue();

        /*
        * @post deletes all elements in the Queue
        */
        ~Queue();

        /** Sees whether this queue is empty.
        @return True if the queue is empty, or false if not. */
        bool isEmpty() const;

        /** Adds a new entry to the back of this queue.
        @post If the operation was successful, newEntry is at the back of the queue.
        @param newEntry  The object to be added as a new entry.
        @throw PrecondViolatedExcep if no memory available for the new item */
        void enqueue(const T& newEntry) throw (PrecondViolatedExcep);

        /** Removes the front of this queue.
        @post If the operation was successful, the front of the queue has been removed.
        @throw PrecondViolatedExcep if the queue is empty when called */
        void dequeue() throw (PrecondViolatedExcep);

        /*
        * @pre assumes the queue is not empty
        * @return the value at the top of the queue
        */
        T peekFront() const throw (PrecondViolatedExcep);

        T getNext() const throw (PrecondViolatedExcep);

        /*
        * @return prints the contents of the queue or empty string if empty
        */
        void print() const;


};

#include "Queue.hpp"

#endif
