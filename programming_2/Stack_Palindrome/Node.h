/**
*	@file : Node.h
*	@author : Sharynne Azhar
*	@date : 02-08-2016
*	Purpose: Header file of the Node class used to create an instance of the Node container class
*/

#ifndef NODE_H
#define NODE_H

template <typename T>
class Node
{
    private:
        T m_value;
        Node<T>* m_next;

    public:
        /**
		* @post Initializes a Node instance
		* @return Initialized m_value to 0 and m_next to null
		**/
		Node();

        /**
		* @post Initializes a Node instance with give value
		* @return a new node instance with set value and m_next to nullptr
		**/
        Node(T value);

		/**
		* @return the value of m_value
		**/
		T getValue() const;

        /**
        * @post Initializes m_value to val
        * @return the new value set
        **/
        void setValue(T val);

		/**
		* @return the address of m_next
		**/
		Node<T>* getNext() const;

		/**
		* @post Initializes the next node
		* @return new node pointer
		**/
		void setNext(Node<T>* prev);
};

#include "Node.hpp"

#endif
