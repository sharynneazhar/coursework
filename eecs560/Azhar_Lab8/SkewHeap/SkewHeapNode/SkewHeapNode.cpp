/**
*	@file   : SkewHeapNode.cpp
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Implementation of the SkewHeapNode class
*/

template<typename T>
SkewHeapNode<T>::SkewHeapNode() {
  leftChildPtr = nullptr;
  rightChildPtr = nullptr;
}

template<typename T>
SkewHeapNode<T>::SkewHeapNode(const T& val) {
  m_value = val;
  leftChildPtr = nullptr;
  rightChildPtr = nullptr;
}

template<typename T>
SkewHeapNode<T>::SkewHeapNode(const T& val, SkewHeapNode<T>* leftPtr, SkewHeapNode<T>* rightPtr) {
  m_value = val;
  leftChildPtr = leftPtr;
  rightChildPtr = rightPtr;
}

template<typename T>
void SkewHeapNode<T>::setValue(const T& val) {
  m_value = val;
}

template<typename T>
T SkewHeapNode<T>::getValue() const {
  return m_value;
}

template<typename T>
void SkewHeapNode<T>::setLeftChildPtr(SkewHeapNode<T>* leftPtr) {
  leftChildPtr = leftPtr;
}

template<typename T>
void SkewHeapNode<T>::setRightChildPtr(SkewHeapNode<T>* rightPtr) {
  rightChildPtr = rightPtr;
}

template<typename T>
SkewHeapNode<T>* SkewHeapNode<T>::getLeftChildPtr() const {
  return leftChildPtr;
}

template<typename T>
SkewHeapNode<T>* SkewHeapNode<T>::getRightChildPtr() const {
  return rightChildPtr;
}
