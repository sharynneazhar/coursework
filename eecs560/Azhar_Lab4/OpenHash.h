/**
*	@file   : OpenHash.h
*	@author : Sharynne Azhar
*	@date   : 02-16-2017
* @desc   : Implementation of open hashing algorithm
*/

#ifndef OPEN_HASH_H
#define OPEN_HASH_H

#include <iostream>

#include "OpenHashNode.h"

template <typename T>
class OpenHash {
  private:

    int m_tableSize;

    OpenHashNode<T>** openHashTable;

    int hash(T value) {
      return value % m_tableSize;
    }

    bool find(T value) {
      int key = hash(value);
      OpenHashNode<T>* currNode = openHashTable[key];
      while (currNode) {
        if (currNode->getValue() == value)
          return true;
        currNode = currNode->getNext();
      }
      return false;
    }

  public:

    OpenHash(T tableSize) {
      m_tableSize = tableSize;
      openHashTable = new OpenHashNode<T>*[m_tableSize];
      for (int i = 0; i < m_tableSize; i++) {
        openHashTable[i] = nullptr;
      }
    }

    virtual ~OpenHash() {
      for (int i = 0; i < m_tableSize; i++) {
        OpenHashNode<T>* currNode = openHashTable[i];
        while (currNode) {
          OpenHashNode<T>* nodeToDelete = currNode;
          currNode = currNode->getNext();
          delete nodeToDelete;
        }
      }
      delete [] openHashTable;
    }

    void insertValue(T value) {
      if (find(value)) {
        return;
      }

      int key = hash(value);
      OpenHashNode<T>* newNode = new OpenHashNode<T>(value);
      if (!openHashTable[key]) {
        openHashTable[key] = newNode;
      } else {
        newNode->setNext(openHashTable[key]);
        openHashTable[key]->setPrev(newNode);
        openHashTable[key] = newNode;
      };
    }

    void deleteValue(T value) {
      if (!find(value)) {
        std::cout << "\nValue is not in list.\n";
        return;
      }

      int key = hash(value);
      OpenHashNode<T>* currNode = openHashTable[key];
      while (currNode) {
        OpenHashNode<T>* nextNode = currNode->getNext();
        if (currNode->getValue() == value) {
          if (!currNode->getPrev()) {
            openHashTable[key] = currNode->getNext();
          } else {
            currNode->getPrev()->setNext(currNode->getNext());
          }
          delete currNode;
        }
        currNode = nextNode;
      }
    }

    void printList() {
      for (int i = 0; i < m_tableSize; i++) {
        std::cout << i << ": ";
        OpenHashNode<T>* currNode = openHashTable[i];
        while (currNode) {
          std::cout << currNode->getValue() << " ";
          currNode = currNode->getNext();
        }
        std::cout << std::endl;
      }
    }

};

#endif
