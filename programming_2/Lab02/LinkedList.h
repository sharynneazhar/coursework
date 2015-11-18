/**
*	@file : LinkedList.h
*	@author : Sharynne Azhar
*	@date : 2015.09.17
*	@brief: Header file of LinkedList class. Used to create a linked list instance of multiple objects
*/

#ifndef LINKEDLIST_H
#define LINKEDLIST_H

#include "Node.h"

class LinkedList {
	private: 
		int m_size;
		Node* m_front;
		
	public:
		/**
		* @pre None 
		* @post Creates and initializes a LinkedList instance
		* @return Initialized m_size to 0 and m_front to null
		**/
		LinkedList();
		
		/**
		* @pre None 
		* @post Removes all nodes in list
		* @return Empty list
		**/
		~LinkedList();
		
		/**
		* @pre None 
		* @post None
		* @return True if the list is empty and false otherwise
		**/
		bool isEmpty() const;
		
		/**
		* @pre None 
		* @post None
		* @return Returns value of m_size 
		**/
		int size() const;
		
		/**
		* @pre An integer value is declared by user
		* @post None
		* @return Returns true if value is in the list or false otherwise
		**/
		bool search(int value) const;

		/**
		* @pre None 
		* @post Creates a list of contents
		* @return Returns each value found in the list
		**/
		void printList() const;
		
		/**
		* @pre Value is integer declared by user 
		* @post Creates a new node and adds to the end of the Linked List
		* @return None
		**/
		void addBack(int value);

		/**
		* @pre Value is integer declared by user 
		* @post Creates a new node and adds to the front of the Linked list
		* @return
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
};

#endif
