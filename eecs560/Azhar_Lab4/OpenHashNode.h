/**
*	@file   : OpenHashNode.h
*	@author : Sharynne Azhar
*	@date   : 02-16-2017
* @desc   : Holds the node object for open hashing
*/

#ifndef OPEN_HASH_NODE_H
#define OPEN_HASH_NODE_H

template <typename T>
class OpenHashNode {
  private:
    T m_value;
    OpenHashNode<T>* m_prev;
    OpenHashNode<T>* m_next;

  public:
    OpenHashNode() {
      m_value = T();
      m_prev = nullptr;
      m_next = nullptr;
    }

    OpenHashNode(T value) {
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

    OpenHashNode<T>* getPrev() const {
      return m_prev;
    }

    void setPrev(OpenHashNode<T>* prev) {
      m_prev = prev;
    }

    OpenHashNode<T>* getNext() const {
      return m_next;
    }

    void setNext(OpenHashNode<T>* next) {
      m_next = next;
    }
};

#endif
