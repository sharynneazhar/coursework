/**
*	@file : HashTable.h
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

#ifndef HASHTABLE_H
#define HASHTABLE_H

#include "Node.h"
#include <iostream>

template <typename T>
class HashTable {
  private:
    int m_tableSize;
    Node<T>** hashTable;

    int hash(T value);
    bool find(T value);

  public:
    HashTable(T tableSize);
    virtual ~HashTable();

    void insertValue(T value);
    void deleteValue(T value);
    void printList();

};

#include "HashTable.hpp"

#endif
