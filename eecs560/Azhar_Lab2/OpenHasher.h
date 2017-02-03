/**
*	@file : OpenHasher.h
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

#ifndef OPENHASHER_H
#define OPENHASHER_H

#include "Node.h"
#include <iostream>

template <typename T>
class OpenHasher {
  private:
    int m_tableSize;
    Node<T>** hashTable;

    int hash(T value);
    bool find(T value);

  public:
    OpenHasher();
    OpenHasher(T tableSize);
    ~OpenHasher();

    void insertValue(T value);
    void deleteValue(T value);
    void printList();

};

#include "OpenHasher.hpp"

#endif
