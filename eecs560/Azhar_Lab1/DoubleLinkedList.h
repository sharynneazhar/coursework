/**
*	@file : DoubleLinkedList.h
*	@author : Sharynne Azhar
*	@date : 01-24-2017
*/

#ifndef DOUBLELINKEDLIST_H
#define DOUBLELINKEDLIST_H

#include "Node.h"

#include <iostream>

template <typename T>
class DoubleLinkedList {
  private:
    Node<T>* m_front;
    Node<T>* m_back;
    int m_size;

    bool insertHelper(Node<T>* currPtr, Node<T>* newNode);

  public:
    DoubleLinkedList();
    ~DoubleLinkedList();

    bool isEmpty() const;
    void insert(T value);
    void remove(T value);
    void reverse();
    void print();

};

#include "DoubleLinkedList.hpp"

#endif
