/**
*	@file   : BinarySearchTree.h
*	@author : Sharynne Azhar
*	@date   : 02-27-2017
* @desc   : Header file of BinarySearchTree class
*/

#ifndef BINARY_SEARCH_TREE_H
#define BINARY_SEARCH_TREE_H

#include <iostream>
#include <stdexcept>

#include "BinaryNode.h"
#include "Queue.h"

template<typename T>
class BinarySearchTree {
  private:
    BinaryNode<T>* rootPtr;

    void destroyTree(BinaryNode<T>* subTreePtr);

    BinaryNode<T>* insertInorder(BinaryNode<T>* subTreePtr, BinaryNode<T>* newNodePtr);

    BinaryNode<T>* removeItem(BinaryNode<T>* subTreePtr, const T item, bool& isRemoved);

    BinaryNode<T>* removeNode(BinaryNode<T>* nodeToDelete);

    BinaryNode<T>* removeMinInRightSubTree(BinaryNode<T>* nodeToDelete, T& inorderSuccessor);

    BinaryNode<T>* searchHelper(BinaryNode<T>* subTreePtr, const T& item) const;

    T findMin(BinaryNode<T>* subTreePtr);

    T findMax(BinaryNode<T>* subTreePtr);

    void preorderHelper(BinaryNode<T>* subTreePtr) const;

    void inorderHelper(BinaryNode<T>* subTreePtr) const;


  public:
    BinarySearchTree();

    virtual ~BinarySearchTree();

    void insertItem(const T& item);

    void removeItem(const T& item);

    BinaryNode<T>* search(const T& item);

    void deleteMin();

    void deleteMax();

    void preorder() const;

    void inorder() const;

    void levelorder() const;

};

#include "BinarySearchTree.cpp"

#endif
