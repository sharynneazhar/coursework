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
  if (!m_front) {
    m_front = new Node<T>(value);
  } else {
    insertHelper(m_front, value);
  }
}

template <typename T>
bool DoubleLinkedList<T>::insertHelper(Node<T>* currNode, T value) {
  if (currNode->getValue() != value) {
    if (!currNode->getNext()) {
      Node<T>* newNode = new Node<T>(value);
      currNode->setNext(newNode);
      newNode->setPrev(currNode);
      return true;
    }
    return insertHelper(currNode->getNext(), value);
  }
  return false;
}

template <typename T>
void DoubleLinkedList<T>::deleteValue(T value) {
  if (!deleteHelper(m_front, value))
    std::cout << "\nValue not found.\n";
}

template <typename T>
bool DoubleLinkedList<T>::deleteHelper(Node<T>* currNode, const T value) {
  if (currNode) {
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
  return false;
}

template <typename T>
void DoubleLinkedList<T>::reverseList() {

}

template <typename T>
void DoubleLinkedList<T>::printList() {
  Node<T>* currNode = m_front;
  while (currNode) {
    std::cout << currNode->getValue() << " ";
    currNode = currNode->getNext();
  }
  std::cout << std::endl;
}
