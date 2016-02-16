/**
*	@file : StackInterface.h
*	@author : Sharynne Azhar
*	@date : 02-15-2016
*	Purpose: Serves as an interface that the Stack Class can use
*/

#ifndef STACKINTERFACE_H
#define STACKINTERFACE_H

#include "Node.h"
#include "PreconditionViolationException.h"

#include <iostream>
#include <stdexcept>
#include <vector>

template <typename T>
class StackInterface
{
    public:

        /*
        * @post eliminates stuff from the heap (destructor)
        */
        virtual ~StackInterface(){};

        /*
        * @return true if stack is not empty, false otherwise
        */
        virtual bool isEmpty() const = 0;

        /*
        * @post entry added to the top of the stack
        */
        virtual void push(const T value) = 0;

        /*
        * @pre assumes the stack is not empty
        * @post top of the stack is removed
        */
        virtual void pop() throw(PreconditionViolationException) = 0;

        /*
        * @pre assumes the stack is not empty
        * @return the value at the top of the stack
        */
        virtual T peek() const throw(PreconditionViolationException) = 0;

        /*
        * @return the size of the stack
        */
        virtual int size() const = 0;

        /*
        * @post copies the stack to a vector (used for testing)
        * @return a vector with a copy of the stack
        */
        virtual std::vector<T> toVector() const = 0;
};

#endif
