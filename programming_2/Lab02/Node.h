/**
*	@file : Node.h
*	@author : Sharynne Azhar
*	@date : 02-08-2016
*	Purpose: Header file of the Node class used to create an instance of the Node container class
*/

#ifndef NODE_H
#define NODE_H

class Node
{
    private:
        int m_value;
        Node* m_next;

    public:
        /**
		* @post Initializes a Node instance
		* @return Initialized m_value to 0 and m_next to null
		**/
		Node();

		/**
		* @return the value of m_value
		**/
		int getValue() const;

        /**
        * @post Initializes m_value to val
        * @return the new value set
        **/
        void setValue(int val);

		/**
		* @return the address of m_next
		**/
		Node* getNext() const;

		/**
		* @post Initializes the next node
		* @return new node pointer
		**/
		void setNext(Node* prev);
};

#endif
