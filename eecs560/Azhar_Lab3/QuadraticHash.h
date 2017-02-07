/**
*	@file : QuadraticHash.h
*	@author : Sharynne Azhar
*	@date : 02-07-2017
*/

#ifndef QUADRATIC_HASH_H
#define QUADRATIC_HASH_H

#include "Node.h"

#include <iostream>

template <typename T>
class QuadraticHash {
  private:
    int m_tableSize;
    Node<T>** hashTable;

    int hash(T value);
    bool find(T value);

  public:
    QuadraticHash(T tableSize);
    ~QuadraticHash();
    void insertValue(T value);
    void deleteValue(T value);
    void printList();
};

#include "QuadraticHash.hpp"

#endif
