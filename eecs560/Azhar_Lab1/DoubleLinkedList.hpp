/**
*	@file : DoubleLinkedList.hpp
*	@author : Sharynne Azhar
*	@date : 01-24-2017
*/

template <typename T>
DoubleLinkedList<T>::DoubleLinkedList() {
  m_front = nullptr;
}

template <typename T>
DoubleLinkedList<T>::~DoubleLinkedList() {
  while (m_front) {
    Node<T>* nodeToDelete = m_front;
    m_front = nodeToDelete->getNext();
    delete nodeToDelete;
  }
}

template <typename T>
void DoubleLinkedList<T>::insertValue(T value) {
  Node<T>* newNode = new Node<T>(value);
  if (!m_front) {
    m_front = newNode;
  } else {
    insertHelper(m_front, newNode);
  }
}

template <typename T>
bool DoubleLinkedList<T>::insertHelper(Node<T>* currNode, Node<T>* newNode) {
  if (currNode->getValue() != newNode->getValue()) {
    if (!currNode->getNext()) {
      currNode->setNext(newNode);
      newNode->setPrev(currNode);
      return true;
    }
    return insertHelper(currNode->getNext(), newNode);
  }
  return false;
}

template <typename T>
void DoubleLinkedList<T>::deleteValue(T value) {
  if (!m_front) {
    std::cout << "\nList empty\n";
  } else {
    deleteHelper(m_front, value);
  }
}

template <typename T>
bool DoubleLinkedList<T>::deleteHelper(Node<T>* currNode, const T value) {
  if (currNode->getValue() == value) {
    if (m_front == currNode)
      m_front = currNode->getNext();
    if (currNode->getPrev())
      currNode->getPrev()->setNext(currNode->getNext());
    if (currNode->getNext())
      currNode->getNext()->setPrev(currNode->getPrev());
    delete currNode;
    return true;
  }
  return deleteHelper(currNode->getNext(), value);
}

template <typename T>
void DoubleLinkedList<T>::reverseList() {}

template <typename T>
void DoubleLinkedList<T>::printList() {
  Node<T>* currNode = m_front;
  while(currNode != nullptr) {
    std::cout << currNode->getValue() << " ";
    currNode = currNode->getNext();
  }
  std::cout << std::endl;
}
