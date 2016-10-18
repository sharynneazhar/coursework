/**
*	@file : Stack.h
*	@author : Sharynne Azhar
*	@date : 02-17-2016
*	@brief: Header file for the stack class used to store nodes in a stack
*/

#ifndef STACK_H
#define STACK_H

#include "StackInterface.h"

template <typename T>
class Stack : public StackInterface<T>
{
    private:
        int m_size;
        Node<T>* m_top;

    public:
        /*
        * @post initalizes an instance of the Stack class
        */
        Stack();

        /*
        * @post deletes all elements in the stack
        */
        ~Stack();

        /*
        * @return true if stack is not empty, false otherwise
        */
        bool isEmpty() const;

        /*
        * @post entry added to the top of the stack
        */
        void push(const T value);

        /*
        * @pre assumes the stack is not empty
        * @post top of the stack is removed
        */
        void pop() throw(PreconditionViolationException);

        /*
        * @pre assumes the stack is not empty
        * @return the value at the top of the stack
        */
        T peek() const throw(PreconditionViolationException);

        /*
        * @return the size of the stack
        */
        int size() const;

        /*
        * @post copies the stack to a vector (used for testing)
        * @return a vector with a copy of the stack
        */
        std::vector<T> toVector() const;

        /*
        * @return prints the contents of the stack or empty string if empty
        */
        void print(bool isNegative) const;

        /*
        * @pre assumes a valid nonempty stack
        * @return a stack with its contents reversed
        */
        Stack<T> reverse();

        /*
        * @pre assumes a valid nonempty stack
        * @return true if the number is a palindrome, false otherwise
        */
        bool checkPalindrome(Stack<T>& revStack);

};

#include "Stack.hpp"

#endif
