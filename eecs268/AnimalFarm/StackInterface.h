/**
*	@file : StackInterface.h
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Header file for the stack interface 
*/

#ifndef STACKINTERFACE_H
#define STACKINTERFACE_H

#include "Node.h"
#include "PreconditionViolationException.h"

#include <stdexcept>

template <typename T>
class StackInterface
{
public:

    /*
    * @pre none
    * @post none
    * @return none
    */
    virtual ~StackInterface(){};

    /*
    * @pre none
    * @post none
    * @return true if stack is not empty, false otherwise
    */
    virtual bool isEmpty() const = 0;

    /*
    * @pre none
    * @post entry added to the top of the stack
    * @return none
    */
    virtual void push(const T newEntry) = 0;

    /*
    * @pre assumes the stack is not empty
    * @post top of the stack is removed
    * @return none
    */
    virtual void pop() throw(PreconditionViolationException) = 0;

    /*
    * @pre assumes the stack is not empty
    * @post none
    * @return the value at the top of the stack
    */
    virtual T peek() const throw(PreconditionViolationException) = 0;

    /*
    * @pre none
    * @post none
    * @return the size of the stack
    */
    virtual int size() const = 0;

    /*
    * @pre none
    * @post none
    * @return prints the contents of the stack or empty string if empty
    */
    virtual void print() const = 0;
};

#endif
