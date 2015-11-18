/**
*	@file : DoubleLinkedList.h
*	@author : Sharynne Azhar
*	@date : 2015.09.21
*	@brief: Header file for templated DoubleLinkedList class used to store linked objects.
*/

#ifndef DOUBLELINKEDLIST_H
#define DOUBLELINKEDLIST_H

#include "Node.h"
#include <stdexcept>

template <typename T>
class DoubleLinkedList
{
private:
  Node<T>* m_front;
  Node<T>* m_back;
  int m_size;

public:
  /*
  * @pre None
  * @post Initializes an instance of DoubleLinkedList
  * @return Initialized pointers to nullptr and size to 0
  */
  DoubleLinkedList();

  /*
  * @pre None
  * @post Removes instaces of DoubleLinkedList
  * @return new list with size decreased by 1
  */
  ~DoubleLinkedList();

  /*
  * @pre None
  * @post None
  * @return true if m_size is 0, false otherwise
  */
  bool isEmpty() const;

  /*
  * @pre none
  * @post none
  * @return size of the list
  */
  int size() const;

  
  /* 
  * @pre Assumes value is of type T
  * @post Puts the value of some type T in a node and 
  *       puts that node at the front of the list and 
  *       increases m_size by 1
  * @return new front node with value type T
  */
  void pushFront(T value);

  /*
  * @pre Assumes value is of type T
  * @post Puts the value of some type T in a node and 
  *       puts that node at the back of the list and 
  *       increases m_size by 1
  * @return new last node with value type T
  */
  void pushBack(T value);

  /*
  * @pre Requires that list is not empty
  * @post Removes the last element on the list
  * @return true if back element is removed, false otherwise
  */
  bool removeBack();

  /*
  * @pre Requires that list is not empty
  * @post Removes the first element on the list
  * @return true if first element is removed, false otherwise
  */
  bool removeFront();

  /*
  * @pre Passed value is type T 
  * @post Deletes node containing the passed value and 
  *       sets m_front and m_back to null
  * @return true if node was removed, false otherwise
  */
  bool remove(T value);

  /*
  * @pre Assumes listValue is in the list of nodes 
  * @post Inserts newValue infront of listValue and size increases by 1
  * @return list with newValue in it and size +1 
  */
  void insertAhead(T listValue, T newValue) throw (std::runtime_error);
 
  /*
  * @pre Assumes listValue is in the list of nodes 
  * @post Inserts newValue behind of listValue and size increases by 1
  * @return list with newValue in it and size +1 
  */
  void insertBehind(T listValue, T newValue) throw (std::runtime_error);
  
  /*
  * @pre Assumes T is comparable with == operator 
  * @post none
  * @return A pointer to the node containing value or nullptr if not found
  */
  Node<T>* find(T value) const;

  /**
    * @pre Assumes the type T is overloaded to be printable using << 
    * @post Creates a list of contents
    * @return Returns each value found in the list
  **/
  void printList() const;

};
#include "DoubleLinkedList.hpp"
#endif
