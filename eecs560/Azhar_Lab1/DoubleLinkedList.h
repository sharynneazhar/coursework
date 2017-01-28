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

    bool insertHelper(Node<T>* currNode, Node<T>* newNode);
    bool deleteHelper(Node<T>* currNode, T value);

  public:
    DoubleLinkedList();
    ~DoubleLinkedList();

    void insertValue(T value);
    void deleteValue(T value);
    void reverseList();
    void printList();

};

#include "DoubleLinkedList.hpp"

#endif
