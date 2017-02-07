/**
*	@file : Node.h
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

#ifndef NODE_H
#define NODE_H

template <typename T>
class Node {
  private:
    T m_value;
    Node<T>* m_prev;
    Node<T>* m_next;

  public:
    Node() {
      m_value = T();
      m_prev = nullptr;
      m_next = nullptr;
    }

    Node(T value) {
      m_value = value;
      m_prev = nullptr;
      m_next = nullptr;
    }

    T getValue() const {
      return m_value;
    }

    T setValue(T val) {
      m_value = val;
    }

    Node<T>* getPrev() const {
      return m_prev;
    }

    void setPrev(Node<T>* prev) {
      m_prev = prev;
    }

    Node<T>* getNext() const {
      return m_next;
    }

    void setNext(Node<T>* next) {
      m_next = next;
    }
};

#endif
