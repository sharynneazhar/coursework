//  Created by Frank M. Carrano and Tim Henry.
//  Copyright (c) 2013 __Pearson Education__. All rights reserved.
//  Modified (JRM): Use Exceptions

/** Listing 13-1.
 @file QueueInterface.h */

#ifndef _QUEUE_INTERFACE
#define _QUEUE_INTERFACE

#include "Node.h"
#include "PrecondViolatedExcep.h"

template<class T>
class QueueInterface
{
public:
    
   /** Sees whether this queue is empty.
    @return True if the queue is empty, or false if not. */
   virtual bool isEmpty() const = 0;

   /** Adds a new entry to the back of this queue.
    @post If the operation was successful, newEntry is at the back of the queue.
    @param newEntry  The object to be added as a new entry.
    @throw PrecondViolatedExcep if no memory available for the new item */

   virtual void enqueue(const T& newEntry) throw (PrecondViolatedExcep) = 0;

   /** Removes the front of this queue.
    @post If the operation was successful, the front of the queue has been removed.
    @throw PrecondViolatedExcep if the queue is empty when called */

   virtual void dequeue() throw (PrecondViolatedExcep) = 0;

   /** Returns the front of this queue.
    @pre The queue is not empty.
    @post The front of the queue has been returned, and the queue is unchanged.
    @throw PrecondViolatedExcep if the queue is empty when called */

   virtual T peekFront() const throw (PrecondViolatedExcep) = 0;

}; // end QueueInterface
#endif
