/**
*	@file   : BinaryNode.h
*	@author : Sharynne Azhar
*	@date   : 02-27-2017
* @desc   : A class of nodes for a binary tree
*/

#ifndef BINARY_NODE_H
#define BINARY_NODE_H

template<typename T>
class BinaryNode {
  private:
    T _item;
    BinaryNode<T>* leftChildPtr;
    BinaryNode<T>* rightChildPtr;

  public:
    BinaryNode() : leftChildPtr(nullptr), rightChildPtr(nullptr) {}

    BinaryNode(const T& item) {
      _item = item;
      leftChildPtr = nullptr;
      rightChildPtr = nullptr;
    }

    BinaryNode(const T& item, BinaryNode<T>* leftPtr, BinaryNode<T>* rightPtr) {
      _item = item;
      leftChildPtr = leftPtr;
      rightChildPtr = rightPtr;
    }

    bool isLeaf() const {
      return (leftChildPtr == nullptr && rightChildPtr == nullptr);
    }

    bool isOnlyChild() const {
      return ((leftChildPtr == nullptr && rightChildPtr != nullptr) ||
        (leftChildPtr != nullptr && rightChildPtr == nullptr));
    }

    void setItem(const T& item) {
      _item = item;
    }

    T getItem() {
      return _item;
    }

    BinaryNode<T>* getLeftChildPtr() const {
      return leftChildPtr;
    }

    void setLeftChildPtr(BinaryNode<T>* leftPtr) {
      leftChildPtr = leftPtr;
    }

    BinaryNode<T>* getRightChildPtr() const {
      return rightChildPtr;
    }

    void setRightChildPtr(BinaryNode<T>* rightPtr) {
      rightChildPtr = rightPtr;
    }

};

#endif
