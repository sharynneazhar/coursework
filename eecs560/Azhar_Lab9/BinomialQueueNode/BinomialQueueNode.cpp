/**
*	@file   : BinomialQueueNode.cpp
*	@author : Sharynne Azhar
*	@date   : 04-03-2017
* @desc   : Implementation for the BinomialQueueNode class
*/

template<typename T>
BinomialQueueNode<T>::BinomialQueueNode() {
  firstChildPtr = nullptr;
  leftSiblingPtr = this;
  rightSiblingPtr = nullptr;
}

template<typename T>
BinomialQueueNode<T>::BinomialQueueNode(const T& val) {
  m_value = val;
  m_order = 0;
  firstChildPtr = nullptr;
  leftSiblingPtr = this;
  rightSiblingPtr = nullptr;
}

template<typename T>
T BinomialQueueNode<T>::getValue() const {
  return m_value;
}

template<typename T>
void BinomialQueueNode<T>::setValue(const T& val) {
  m_value = val;
}

template<typename T>
int BinomialQueueNode<T>::getOrder() const {
  return m_order;
}

template<typename T>
void BinomialQueueNode<T>::setOrder(const T& order) {
  m_order = order;
}

template<typename T>
BinomialQueueNode<T>* BinomialQueueNode<T>::getFirstChildPtr() const {
  return firstChildPtr;
}

template<typename T>
void BinomialQueueNode<T>::setFirstChildPtr(BinomialQueueNode<T>* childPtr) {
  firstChildPtr = childPtr;
}

template<typename T>
BinomialQueueNode<T>* BinomialQueueNode<T>::getLeftSiblingPtr() const {
  return leftSiblingPtr;
}

template<typename T>
void BinomialQueueNode<T>::setLeftSiblingPtr(BinomialQueueNode<T>* leftPtr) {
  leftSiblingPtr = leftPtr;
}

template<typename T>
BinomialQueueNode<T>* BinomialQueueNode<T>::getRightSiblingPtr() const {
  return rightSiblingPtr;
}

template<typename T>
void BinomialQueueNode<T>::setRightSiblingPtr(BinomialQueueNode<T>* rightPtr) {
  rightSiblingPtr = rightPtr;
}
