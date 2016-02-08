/**
*	@file : Node.cpp
*	@author : Sharynne Azhar
*	@date : 02-08-2016
*	Purpose: implementation of the Node class
*/

#include "Node.h"

Node::Node()
{
    m_value = 0;
    m_next = nullptr;
}

int Node::getValue() const
{
    return m_value;
}

void Node::setValue(int val)
{
    m_value = val;
}

Node* Node::getNext() const
{
    return m_next;
}

void Node::setNext(Node* prev)
{
    m_next = prev;
}
