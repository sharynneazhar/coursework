/**
*	@file : LinkedList.h
*	@author : Sharynne Azhar
*	@date : 02-08-2016
*	Purpose: Header file of the LinkedList class used to control a list or sequence of Nodes
*/

#ifndef LINKEDLIST_H
#define LINKEDLIST_H

#include <iostream>
#include <vector>
#include "Node.h"

class LinkedList
{
    private:
        Node* m_front;
        int m_size;

    public:
        /**
		* @post Creates and initializes a LinkedList instance
		* @return Initialized m_size to 0 and m_front to null
		**/
        LinkedList();

        /**
		* @post Removes all nodes in list
		* @return Empty list
		**/
        ~LinkedList();

        /**
        * @return Returns value of m_size
        **/
        int size() const;

        /**
		* @return True if the list is empty and false otherwise
		**/
        bool isEmpty() const;

        /**
		* @pre An integer value is declared by user
		* @return Returns true if value is in the list or false otherwise
		**/
        bool search(int value) const;

        /**
		* @post Creates a list of contents
		* @return Returns each value found in the list
		**/
        void printList() const;

        /**
		* @pre Value is integer declared by user
		* @post Creates a new node and adds to the end of the Linked List
		**/
        void addBack(int value);

        /**
		* @pre Value is integer declared by user
		* @post Creates a new node and adds to the front of the Linked list
		**/
        void addFront(int value);

        /**
		* @pre List is not empty
		* @post Removes the last node in the linked list
		* @return Returns true if node is removed and false if it is empty
		**/
        bool removeBack();

        /**
		* @pre List is not empty
		* @post Removes the first node in the linked list and m_front set to next node
		* @return Returns true if node is removed and false if it is empty
		**/
        bool removeFront();

        /**
		* @pre List is not empty
		* @post linkedlist is copied in a vector
		* @return Returns a vector of linkedlist contents
		**/
        std::vector<int> toVector() const;
};

#endif
