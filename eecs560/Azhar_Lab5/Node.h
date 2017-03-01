/**
*	@file   : Node.h
*	@author : Sharynne Azhar
*	@date   : 02-28-2017
* @desc   : Holds the node object for open hashing (single-linked)
*/

#ifndef NODE_H
#define NODE_H

template <typename T>
class Node {
  
  private:
    T m_value;

    Node<T>* m_next;

  public:
    Node() {
      m_value = T();
      m_next = nullptr;
    }

    Node(const T value) {
      m_value = value;
      m_next = nullptr;
    }

    Node(const T value, Node<T>* nextNode) {
      m_value = value;
      m_next = nextNode;
    }

    T getValue() const {
      return m_value;
    }

    T setValue(const T val) {
      m_value = val;
    }

    Node<T>* getNext() const {
      return m_next;
    }

    void setNext(Node<T>* next) {
      m_next = next;
    }
};

#endif
