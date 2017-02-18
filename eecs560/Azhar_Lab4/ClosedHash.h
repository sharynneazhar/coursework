/**
*	@file   : ClosedHash.h
*	@author : Sharynne Azhar
*	@date   : 02-17-2017
* @desc   : Implementation of closed hashing algorithm
*/

#ifndef CLOSED_HASH_H
#define CLOSED_HASH_H

#include <iostream>
#include <string>

#include "ClosedHashNode.h"

template <typename T>
class ClosedHash {
  private:

    char m_hashMethod;

    int m_tableSize;

    ClosedHashNode<T>* closedHashTable;

    T hash(const T value, T iteration) {
      // quadratic hashing
      if (m_hashMethod == 'Q')
        return (((value % m_tableSize) + (iteration * iteration)) % m_tableSize);

      // double hashing
      T p = 5;
      return ((value % m_tableSize) + (iteration * (p - (value % p)))) % m_tableSize;
    }

    int findPosition(const T value) {
      T key, iter = 0;
      do {
        key = hash(value, iter);
        if (closedHashTable[key].getValue() == value)
          return key;
        iter++;
      } while ((closedHashTable[key].insertable()) && (iter < m_tableSize));
      return -1;
    }

  public:

    ClosedHash(int tableSize, char hashMethod) {
      m_tableSize = tableSize;
      m_hashMethod = hashMethod;
      closedHashTable = new ClosedHashNode<T>[tableSize];
      for (int i = 0; i < m_tableSize; i++) {
        closedHashTable[i] = ClosedHashNode<T>();
      }
    }

    virtual ~ClosedHash() {
      delete [] closedHashTable;
    }

    bool contains(const T value) {
      T pos = findPosition(value);
      return (pos != -1);
    }

    void insertValue(const T value) {
      if (contains(value)) {
        return;
      }

      T key, iter = 0;
      do {
        key = hash(value, iter);
        if (closedHashTable[key].insertable()) {
          closedHashTable[key].setValue(value);
          return;
        }
        iter++;
      } while (iter < 60000);
    }

    void deleteValue(const T value) {
      if (!contains(value)) {
        return;
      }

      int key = findPosition(value);
      closedHashTable[key].setValue(-1);
      closedHashTable[key].setFlag(true);
    }

};

#endif
