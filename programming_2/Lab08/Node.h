/**
*	@file : Node.h
*	@author : Sharynne Azhar
*	@date : 2015.11.08
*	@brief: Header file for templated Node class used to store objects.
*/

#ifndef NODE_H
#define NODE_H

template <typename T>
class Node
{
private:
  T m_value;
  Node<T>* m_left; // points to the left subtree
  Node<T>* m_right; // points to the right subtree

public:
  /*
  * @pre Assumes that the type T has a parameterless constructor
  * @post Initializes an instance for templated Node object
  * @return leaves m_value alone, m_left and m_right to nullptr
  */
  Node();

  /*
  * @pre Assumes that the type T has a parameterless constructor
  * @post create a deep copy of the other node
  * @return Initialized m_value to T(), m_left and m_right to nullptr
  */
  Node(const Node<T>& other);

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
  void setValue(T value);

  /*
  * @pre None
  * @post None
  * @return left node
  */
  Node<T>* getLeft() const;

  /*
  * @pre Assumes left subtree is has the type templated Node pointer
  * @post m_left changes to left
  * @return None
  */
  void setLeft(Node<T>* left);

  /*
  * @pre None
  * @post None
  * @return right node
  */
  Node<T>* getRight() const;

  /*
  * @pre Assumes right subtree is has the type templated Node pointer
  * @post m_right changes to right
  * @return None
  */
  void setRight(Node<T>* right);
};
#include "Node.hpp"
#endif
