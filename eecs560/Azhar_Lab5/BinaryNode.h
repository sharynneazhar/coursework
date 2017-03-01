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
    BinaryNode();

    BinaryNode(const T& item);

    BinaryNode(const T& item, BinaryNode<T>* leftPtr, BinaryNode<T>* rightPtr);

    bool isLeaf() const;

    bool isOnlyChild() const;

    void setItem(const T& item);

    T getItem();

    BinaryNode<T>* getLeftChildPtr() const;

    void setLeftChildPtr(BinaryNode<T>* leftPtr);

    BinaryNode<T>* getRightChildPtr() const;

    void setRightChildPtr(BinaryNode<T>* rightPtr);

};

#include "BinaryNode.cpp"

#endif
