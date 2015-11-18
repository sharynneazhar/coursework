/**
*	@file : LinkedList.cpp
*	@author : Sharynne Azhar
*	@date : 2015.09.17
*	@brief: Implementation file of LinkedList class
*/

#include "LinkedList.h"
#include "Node.h"

#include <iostream>

using namespace std;

LinkedList::LinkedList() {
	m_size = 0;
	m_front = nullptr;
}

LinkedList::~LinkedList() {
	removeFront();
	removeBack();
}

bool LinkedList::isEmpty() const {
	if (m_size == 0)
	{
		return (true);
	}
	else
	{
		return (false);
	}
}

int LinkedList::size() const {
	return (m_size);
}
	
bool LinkedList::search(int value) const {
	Node* temp = m_front;
	
	if (m_size == 0)
	{
		cout << "List empty." << endl;
		return (false);
	}
	else
	{
		while (temp != nullptr)
		{
			if (value == temp->getValue())
			{
				cout << value << " is in the list." << endl;
				return (true);
			}
			temp = temp->getNext();
		}
		cout << value << " is not in the list." << endl;
		return (false);
	}
}

void LinkedList::printList() const {
	Node* temp = m_front;

	while (temp != nullptr)
	{
		cout << temp -> getValue() << " ";
		temp = temp -> getNext();
	}
}
	
void LinkedList::addBack(int value) {
	Node* temp = nullptr; // use to create new Node instance
	Node* traverse = m_front; // use to track last Node instance
	
	if (m_size == 0)
	{
		m_front = new Node();
		m_front->setValue(value);
		m_size++;
	}
	else if (m_size > 0)
	{
		while (traverse->getNext() != nullptr) 
		{
			traverse = traverse->getNext();
		}
		
		temp = new Node();
		temp->setValue(value);
		traverse->setNext(temp);
		m_size++;
	}
}

void LinkedList::addFront(int value) {
	Node* temp = new Node();
	temp->setValue(value);
	
	if (m_front == nullptr)
	{
		m_front = temp;
	}
	else 
	{
		temp->setNext(m_front);
		m_front = temp;
	}
	
	m_size++;
}
	
bool LinkedList::removeBack() {
	Node* nextToLast = nullptr;
	Node* traverse = m_front;
	
	if (isEmpty())
	{
		cout << "List empty." << endl;
		return (false);
	}
	else
	{
		while (traverse->getNext() != nullptr)
		{
			nextToLast = traverse;
			traverse = traverse->getNext();
		}
		
		delete traverse;

		if (nextToLast != nullptr)
		{
			nextToLast->setNext(nullptr);
		}
		m_size--;
	}
	
	return (true);
}

bool LinkedList::removeFront() {
	Node* temp = m_front;
	
	if (isEmpty())
	{
		cout << "List empty." << endl;
		return (false);
	}
	else
	{
		m_front = m_front->getNext();
		delete temp;
		temp = nullptr;
		m_size--;
	}
	
	return (true);
}
