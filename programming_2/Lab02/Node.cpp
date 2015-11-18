/**
*	@file : Node.cpp
*	@author : Sharynne Azhar
*	@date : 2015.09.17
*	@brief: Implementation file of Node class
*/

#include "Node.h"

Node::Node() {
	m_value = 0;
	m_next = nullptr;
}

int Node::getValue() const {
	return (m_value);
}

Node* Node::getNext() const {
	return (m_next);
}
		
void Node::setValue(int val) {
	m_value = val;
}

Node* Node::setNext(Node* prev) {
	m_next = prev;
}
