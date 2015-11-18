/**
*	@file : Node.h
*	@author : Sharynne Azhar
*	@date : 2015.09.28
*	@brief: Header file for templated Node class used to store objects.
*/

#ifndef NODE_H
#define NODE_H

template <typename T>
class Node
{
private:
  T m_value;
  Node<T>* m_previous;
  Node<T>* m_next;

public:
  /*
  * @pre Assumes that the type T has a parameterless constructor
  * @post Initializes an instance for templated Node object
  * @return Initialized m_value to T(), m_previous and m_next to nullptr
  */
  Node();

  /*
  * @pre None
  * @post None
  * @return m_value of type T
  */
  T getValue() const;

  /*
  * @pre Assumes val has type T
  * @post m_value changes to val
  * @return none
  */
  void setValue(T val);

  /*
  * @pre None
  * @post None
  * @return previous node
  */
  Node<T>* getPrev() const;

  /*
  * @pre Assumes prev is has the type templated Node pointer
  * @post m_previous changes to prev
  * @return None
  */
  void setPrev(Node<T>* prev);

  /*
  * @pre None
  * @post None
  * @return next node
  */
  Node<T>* getNext() const;

  /*
  * @pre Assumes next is has the type templated Node pointer
  * @post m_next changes to next
  * @return None
  */
  void setNext(Node<T>* next);
};
#include "Node.hpp"
#endif
