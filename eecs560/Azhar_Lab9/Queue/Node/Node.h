/**
*	@file   : Node.h
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Holds the node object (single-linked)
*/

#ifndef NODE_H
#define NODE_H

template <typename T>
class Node {
  private:
    T m_value;

    Node<T>* m_next;

  public:
    Node();

    Node(const T value);

    Node(const T value, Node<T>* nextNode);

    T getValue() const;

    T setValue(const T val);

    Node<T>* getNext() const;
    
    void setNext(Node<T>* next);
};

#include "Node.cpp"

#endif
