/**
*	@file : DoubleLinkedList.hpp
*	@author : Sharynne Azhar
*	@date : 01-24-2017
*/

template <typename T>
DoubleLinkedList<T>::DoubleLinkedList() {
  m_front = nullptr;
  m_back = nullptr;
  m_size = 0;
}

template <typename T>
DoubleLinkedList<T>::~DoubleLinkedList() {
  while (m_front != nullptr) {
    Node<T>* nodeToDelete = m_front;
    m_front = nodeToDelete->getNext();
    delete nodeToDelete;
  }
}

template <typename T>
bool DoubleLinkedList<T>::isEmpty() const {
  return m_size == 0;
}

template <typename T>
void DoubleLinkedList<T>::insert(T value) {
  bool isAdded = false;
  Node<T>* newNode = new Node<T>(value);
  if (isEmpty()) {
    m_front = newNode;
    isAdded = true;
  } else {
    isAdded = insertHelper(m_front, newNode);
  }

  if (isAdded)
    m_size++;
}

template <typename T>
bool DoubleLinkedList<T>::insertHelper(Node<T>* currPtr, Node<T>* newNode) {
  if (currPtr->getValue() != newNode->getValue()) {
    if (currPtr->getNext() == nullptr) {
      currPtr->setNext(newNode);
    } else {
      insertHelper(currPtr->getNext(), newNode);
    }
  } else {
    std::cout << "\nValue already in list.\n";
    return false;
  }
  return true;
}

template <typename T>
void DoubleLinkedList<T>::remove(T value) {}

template <typename T>
void DoubleLinkedList<T>::reverse() {}

template <typename T>
void DoubleLinkedList<T>::print() {
  Node<T>* currPtr = m_front;
  while(currPtr != nullptr) {
    std::cout << currPtr->getValue() << " ";
    currPtr = currPtr->getNext();
  }
  std::cout << std::endl;
}
