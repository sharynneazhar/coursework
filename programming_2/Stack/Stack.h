#ifndef STACK_H
#define STACK_H

#include "Node.h"
#include "StackInterface.h"
#include "PreconditionViolationException.h"

#include <iostream>
#include <stdexcept>

template <typename T>
class Stack : public StackInterface<T>
{
private:
    int m_size;
    Node<T>* m_top;

public:
    /*
    * @pre none
    * @post deletes all elements in the stack
    * @return none
    */
    Stack();

    /*
    * @pre none
    * @post deletes all elements in the stack
    * @return none
    */
    ~Stack();

    /*
    * @pre none
    * @post none
    * @return true if stack is not empty, false otherwise
    */
    bool isEmpty() const;

    /*
    * @pre none
    * @post entry added to the top of the stack
    * @return none
    */
    void push(const T newEntry);

    /*
    * @pre assumes the stack is not empty
    * @post top of the stack is removed
    * @return none
    */
    void pop() throw(PreconditionViolationException);

    /*
    * @pre assumes the stack is not empty
    * @post none
    * @return the value at the top of the stack
    */
    T peek() const throw(PreconditionViolationException);

    /*
    * @pre none
    * @post none
    * @return the size of the stack
    */
    int size() const;

    /*
    * @pre none
    * @post none
    * @return prints the contents of the stack or empty string if empty
    */
    void print() const;
};

#include "Stack.hpp"
#endif
