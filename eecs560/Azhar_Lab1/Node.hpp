/**
*	@file : Node.hpp
*	@author : Sharynne Azhar
*	@date : 01-24-2017
*/

template <typename T>
Node<T>::Node() {
  m_value = T();
  m_prev = nullptr;
  m_next = nullptr;
}

template <typename T>
Node<T>::Node(T value) {
  m_value = value;
  m_prev = nullptr;
  m_next = nullptr;
}

template <typename T>
T Node<T>::getValue() const {
  return m_value;
}

template <typename T>
T Node<T>::setValue(T val) {
  m_value = val;
}

template <typename T>
Node<T>* Node<T>::getPrev() const {
  return m_prev;
}

template <typename T>
void Node<T>::setPrev(Node<T>* prev) {
  m_prev = prev;
}

template <typename T>
Node<T>* Node<T>::getNext() const {
  return m_next;
}

template <typename T>
void Node<T>::setNext(Node<T>* next) {
  m_next = next;
}
