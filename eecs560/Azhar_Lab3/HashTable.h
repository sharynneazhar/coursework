/**
*	@file : HashTable.h
*	@author : Sharynne Azhar
*	@date : 02-07-2017
*/

#ifndef HASH_TABLE_H
#define HASH_TABLE_H

#include "HashObject.h"

#include <iostream>
#include <string>
#include <iomanip>
#include <stdlib.h>

template <typename T>
class HashTable {
  private:
    char m_hashMethod;
    int m_tableSize;
    HashObject<T>* hashTable;

    int hash(T value, int iteration);
    bool contains(T value);

  public:
    HashTable(T tableSize, char hashMethod);
    virtual ~HashTable();

    void insertValue(T value);
    void deleteValue(T value);
    void printList();
};

#include "HashTable.hpp"

#endif
