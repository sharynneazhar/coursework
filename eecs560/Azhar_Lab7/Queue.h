/**
*	@file   : Queue.h
*	@author : Sharynne Azhar
*	@date   : 02-28-2017
* @desc   : Header file of Queue class for level order traversing
*/

#ifndef QUEUE_H
#define QUEUE_H

#include "Node.h"
#include "PVE.h"

template<typename T>
class Queue {
  private:
    int m_size;
    Node<T>* m_front;
    Node<T>* m_back;

  public:
    Queue() : m_size(0), m_front(nullptr), m_back(nullptr) {}

    virtual ~Queue() {
      while(!isEmpty()) {
        dequeue();
      }
    }

    bool isEmpty() const {
      return m_size == 0;
    }

    void enqueue(const T& item) {
      Node<T>* newNode = new Node<T>(item);

      if (isEmpty()) {
        m_front = newNode;
        m_back = m_front;
      } else {
        m_back->setNext(newNode);
        m_back = newNode;
      }

      m_size++;
    }

    void dequeue() throw (PVE) {
      Node<T>* nodeToDequeue = nullptr;
      if (isEmpty()) {
        throw PVE("\nDequeue failed. ERROR: Queue empty.\n");
      }

      nodeToDequeue = m_front;
      m_front = m_front->getNext();

      m_size--;

      delete nodeToDequeue;
      nodeToDequeue = nullptr;
    }

    T peek() const throw (PVE) {
      if (isEmpty()) {
        throw PVE("\nPeek failed. ERROR: Queue empty.\n");
      }

      return m_front->getValue();
    }

    T getNext() const throw (PVE) {
      if (isEmpty()) {
        throw PVE("\nGet next failed. ERROR: Queue empty.\n");
      }

      return m_front->getNext()->getValue();
    }

    void print() const {
      Node<T>* traverser = m_front;
      std::cout << std::endl;
      while (traverser) {
        std::cout << traverser->getValue() << " ";
        traverser = traverser->getNext();
      }
      std::cout << std::endl;
    }

};

#endif
