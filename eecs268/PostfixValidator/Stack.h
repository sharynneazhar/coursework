/**
*	@file : Stack.h
*	@author : Sharynne Azhar
*	@date : 02-17-2016
*	@brief: Header file for the stack class used to store items in a stack
*/

#ifndef STACK_H
#define STACK_H

#include "StackInterface.h"

const int MAX_STACK = 50;

template <typename T>
class Stack : public StackInterface<T>
{
    private:
        int top;
        T items[MAX_STACK];

    public:
        /*
        * @post initalizes an instance of the Stack class
        */
        Stack();

        /*
        * @return true if stack is not empty, false otherwise
        */
        bool isEmpty() const;

        /*
        * @post entry added to the top of the stack
        */
        void push(const T& newEntry) throw (PrecondViolatedExcep);

        /*
        * @pre assumes the stack is not empty
        * @post top of the stack is removed
        */
        void pop() throw(PrecondViolatedExcep);

        /*
        * @pre assumes the stack is not empty
        * @return the value at the top of the stack
        */
        T peek() const throw(PrecondViolatedExcep);

};

#include "Stack.hpp"

#endif
