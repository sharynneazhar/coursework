/**
*	@file : DoubleLinkedList.hpp
*	@author : Sharynne Azhar
*	@date : 01-24-2017
*/

template <typename T>
DoubleLinkedList<T>::DoubleLinkedList() {
  m_front = nullptr;
  m_back = nullptr;
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
    m_back = insertHelper(m_front, value);
  }
}

template <typename T>
Node<T>* DoubleLinkedList<T>::insertHelper(Node<T>* currNode, T value) {
  if (currNode->getValue() == value) {
    std::cout << "\nValue already in list.\n" << std::endl;
    return currNode;
  }

  if (!currNode->getNext()) {
    Node<T>* newNode = new Node<T>(value);
    currNode->setNext(newNode);
    newNode->setPrev(currNode);
    return newNode;
  }

  return insertHelper(currNode->getNext(), value);
}

template <typename T>
void DoubleLinkedList<T>::deleteValue(T value) {
  if (!deleteHelper(m_front, value))
    std::cout << "\nValue not found.\n";
}

template <typename T>
bool DoubleLinkedList<T>::deleteHelper(Node<T>* currNode, const T value) {
  if (!currNode)
    return false;

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
void DoubleLinkedList<T>::reverseList() {
  if (m_front) {
    m_front = reverseListHelper(m_front);
  }
}

template <typename T>
Node<T>* DoubleLinkedList<T>::reverseListHelper(Node<T>* currNode) {
  if (!currNode)
    return currNode;

  if (!currNode->getNext()) {
    currNode->setPrev(nullptr);
    return currNode;
  }

  Node<T>* tempNode = reverseListHelper(currNode->getNext());
  currNode->getNext()->setNext(currNode);
  currNode->setPrev(currNode->getNext());
  currNode->setNext(nullptr);
  return tempNode;
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
