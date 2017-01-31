/**
*	@file : Node.h
*	@author : Sharynne Azhar
*	@date : 01-24-2017
*/

#ifndef NODE_H
#define NODE_H

template <typename T>
class Node {
  private:
    T m_value;
    Node<T>* m_prev;
    Node<T>* m_next;

  public:
    Node();
    Node(T value);
    T getValue() const;
    T setValue(T val);
    Node<T>* getPrev() const;
    void setPrev(Node<T>* prev);
    Node<T>* getNext() const;
    void setNext(Node<T>* next);
};

#include "Node.hpp"

#endif
