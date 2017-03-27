/**
*	@file   : LeftistHeapNode.cpp
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Implementation for the LeftistHeapNode class
*/

template<typename T>
LeftistHeapNode<T>::LeftistHeapNode() {
  m_rank = -1;
  leftChildPtr = nullptr;
  rightChildPtr = nullptr;
}

template<typename T>
LeftistHeapNode<T>::LeftistHeapNode(const T& val) {
  m_rank = 1;
  m_value = val;
  leftChildPtr = nullptr;
  rightChildPtr = nullptr;
}

template<typename T>
LeftistHeapNode<T>::LeftistHeapNode(const T& val, LeftistHeapNode<T>* leftPtr, LeftistHeapNode<T>* rightPtr) {
  m_rank = 1;
  m_value = val;
  leftChildPtr = leftPtr;
  rightChildPtr = rightPtr;
}

template<typename T>
LeftistHeapNode<T>::LeftistHeapNode(const T& val, const int rank, LeftistHeapNode<T>* leftPtr, LeftistHeapNode<T>* rightPtr) {
  m_rank = rank;
  m_value = val;
  leftChildPtr = leftPtr;
  rightChildPtr = rightPtr;
}

template<typename T>
void LeftistHeapNode<T>::setValue(const T& val) {
  m_value = val;
}

template<typename T>
T LeftistHeapNode<T>::getValue() const {
  return m_value;
}

template<typename T>
void LeftistHeapNode<T>::setRank(const T& rank) {
  m_rank = rank;
}

template<typename T>
int LeftistHeapNode<T>::getRank() const {
  return m_rank;
}

template<typename T>
void LeftistHeapNode<T>::setLeftChildPtr(LeftistHeapNode<T>* leftPtr) {
  leftChildPtr = leftPtr;
}

template<typename T>
LeftistHeapNode<T>* LeftistHeapNode<T>::getLeftChildPtr() const {
  return leftChildPtr;
}

template<typename T>
void LeftistHeapNode<T>::setRightChildPtr(LeftistHeapNode<T>* rightPtr) {
  rightChildPtr = rightPtr;
}

template<typename T>
LeftistHeapNode<T>* LeftistHeapNode<T>::getRightChildPtr() const {
  return rightChildPtr;
}
