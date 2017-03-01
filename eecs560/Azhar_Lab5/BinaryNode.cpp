/**
*	@file   : BinaryNode.cpp
*	@author : Sharynne Azhar
*	@date   : 02-27-2017
* @desc   : Implementation of the Binary Node class
*/

template<typename T>
BinaryNode<T>::BinaryNode() : leftChildPtr(nullptr), rightChildPtr(nullptr) {};

template<typename T>
BinaryNode<T>::BinaryNode(const T& item) : _item(item), leftChildPtr(nullptr), rightChildPtr(nullptr) {};

template<typename T>
BinaryNode<T>::BinaryNode(const T& item, BinaryNode<T>* leftPtr, BinaryNode<T>* rightPtr) :  _item(item), leftChildPtr(leftPtr), rightChildPtr(rightPtr) {};

template<typename T>
bool BinaryNode<T>::isLeaf() const {
  return (leftChildPtr == nullptr && rightChildPtr == nullptr);
}

template<typename T>
bool BinaryNode<T>::isOnlyChild() const {
  return ((leftChildPtr == nullptr && rightChildPtr != nullptr) ||
    (leftChildPtr != nullptr && rightChildPtr == nullptr));
}

template<typename T>
void BinaryNode<T>::setItem(const T& item) {
  _item = item;
}

template<typename T>
T BinaryNode<T>::getItem() {
  return _item;
}

template<typename T>
BinaryNode<T>* BinaryNode<T>::getLeftChildPtr() const {
  return leftChildPtr;
}

template<typename T>
void BinaryNode<T>::setLeftChildPtr(BinaryNode<T>* leftPtr) {
  leftChildPtr = leftPtr;
}

template<typename T>
BinaryNode<T>* BinaryNode<T>::getRightChildPtr() const {
  return rightChildPtr;
}

template<typename T>
void BinaryNode<T>::setRightChildPtr(BinaryNode<T>* rightPtr) {
  rightChildPtr = rightPtr;
}
