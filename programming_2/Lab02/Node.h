/**
*	@file : Node.h
*	@author : Sharynne Azhar
*	@date : 2015.09.17
*	@brief: Header file of Node class. Used to create a Node instance to store objects
*/

#ifndef NODE_H
#define NODE_H

class Node {
	private:
		int m_value;
		Node* m_next;
	
	public:
		/**
		* @pre None 
		* @post Creates and initializes a Node instance
		* @return Initialized m_value to 0 and m_next to null
		**/
		Node();
		
		/**
		* @pre None 
		* @post None
		* @return the value of m_value
		**/
		int getValue() const;
		
		/**
		* @pre None 
		* @post Noe
		* @return the address of m_next
		**/
		Node* getNext() const;
		
		/**
		* @pre None 
		* @post Initializes m_value to val
		* @return the new value set
		**/
		void setValue(int val);
		
		/**
		* @pre None 
		* @post Initializes the next node
		* @return new node pointer
		**/
		Node* setNext(Node* prev);
};

#endif
