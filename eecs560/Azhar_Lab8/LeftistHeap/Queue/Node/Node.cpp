/**
*	@file   : Node.cpp
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Implementation of the Node class
*/

template <typename T>
Node<T>::Node() {
  m_value = T();
  m_next = nullptr;
}

template <typename T>
Node<T>::Node(const T value) {
  m_value = value;
  m_next = nullptr;
}

template <typename T>
Node<T>::Node(const T value, Node<T>* nextNode) {
  m_value = value;
  m_next = nextNode;
}

template <typename T>
T Node<T>::getValue() const {
  return m_value;
}

template <typename T>
T Node<T>::setValue(const T val) {
  m_value = val;
}

template <typename T>
Node<T>* Node<T>::getNext() const {
  return m_next;
}

template <typename T>
void Node<T>::setNext(Node<T>* next) {
  m_next = next;
}
