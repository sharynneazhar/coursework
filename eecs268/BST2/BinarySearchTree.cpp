//  Subsetted from:
//  Created by Frank M. Carrano and Tim Henry.
//  Copyright (c) 2013 __Pearson Education__. All rights reserved.

/** @file BinarySearchTree.cpp */
#include <iostream>

#include "BinarySearchTree.h"

// PRIVATE HELPER METHODS - IMPLEMENT THESE

template<typename ItemType, typename KeyType>
void BinarySearchTree<ItemType, KeyType>::destroyTree(BinaryNode<ItemType>* subTreePtr)
{
    if (subTreePtr != nullptr) {
        destroyTree(subTreePtr->getLeftChildPtr());
        destroyTree(subTreePtr->getRightChildPtr());
    }

    delete subTreePtr;
}

template<typename ItemType, typename KeyType>
BinaryNode<ItemType>* BinarySearchTree<ItemType,KeyType>::insertInorder(BinaryNode<ItemType>* subTreePtr, BinaryNode<ItemType>* newNodePtr)
{
    if (subTreePtr == nullptr) {
        return newNodePtr;
    } else if (subTreePtr->getItem() > newNodePtr->getItem()) {
        BinaryNode<ItemType>* tempPtr = insertInorder(subTreePtr->getLeftChildPtr(), newNodePtr);
        subTreePtr->setLeftChildPtr(tempPtr);
    } else {
        BinaryNode<ItemType>* tempPtr = insertInorder(subTreePtr->getRightChildPtr(), newNodePtr);
        subTreePtr->setRightChildPtr(tempPtr);
    }

    return subTreePtr;
}

template<typename ItemType, typename KeyType>
BinaryNode<ItemType>* BinarySearchTree<ItemType, KeyType>::findNode(BinaryNode<ItemType>* subTreePtr, const KeyType& target) const
{
    if (subTreePtr == nullptr) {
        return nullptr;
    } else if (subTreePtr->getItem() == target) {
        return subTreePtr;
    } else if (subTreePtr->getItem() < target) {
        return findNode(subTreePtr->getRightChildPtr(), target);
    } else {
        return findNode(subTreePtr->getLeftChildPtr(), target);
    }
}

template<typename ItemType, typename KeyType>
BinaryNode<ItemType>* BinarySearchTree<ItemType, KeyType>::removeHelper(BinaryNode<ItemType>* subTreePtr, const KeyType target, bool& isRemoved) {
    if (subTreePtr == nullptr) {
        isRemoved = false;
        return nullptr;
    } else if (subTreePtr -> getItem() == target) {
        subTreePtr = removeNode(subTreePtr);
        isRemoved = true;
        return subTreePtr;
    } else if (subTreePtr -> getItem() > target) {
        BinaryNode<ItemType>* tempPtr = removeHelper(subTreePtr -> getLeftChildPtr(), target, isRemoved);
        subTreePtr -> setLeftChildPtr(tempPtr);
        return subTreePtr;
    } else {
        BinaryNode<ItemType>* tempPtr = removeHelper(subTreePtr -> getRightChildPtr(), target, isRemoved);
        subTreePtr -> setRightChildPtr(tempPtr);
        return subTreePtr;
    }
}

template<typename ItemType, typename KeyType>
BinaryNode<ItemType>* BinarySearchTree<ItemType, KeyType>::removeNode(BinaryNode<ItemType>* nodeToDeletePtr) {
    if (nodeToDeletePtr->isLeaf()) {
        delete nodeToDeletePtr;
        nodeToDeletePtr = nullptr;
    } else if (nodeToDeletePtr->isLeaf() || !nodeToDeletePtr->isLeaf()) {
        BinaryNode<ItemType>* nodeToConnectPtr;
        if(nodeToDeletePtr->getLeftChildPtr() != nullptr)
            nodeToConnectPtr = nodeToDeletePtr->getLeftChildPtr();
        else
            nodeToConnectPtr = nodeToDeletePtr->getRightChildPtr();

        delete nodeToDeletePtr;
        nodeToDeletePtr = nullptr;
        return nodeToConnectPtr;
    } else {
        ItemType inorderSuccessor;
        BinaryNode<ItemType>* tempPtr = findSuccessor(nodeToDeletePtr->getRightChildPtr(), inorderSuccessor);
        nodeToDeletePtr->setRightChildPtr(tempPtr);
        nodeToDeletePtr->setItem(inorderSuccessor);
    }

    return nodeToDeletePtr;
}

template<typename ItemType, typename KeyType>
BinaryNode<ItemType>* BinarySearchTree<ItemType, KeyType>::findSuccessor(BinaryNode<ItemType>* nodeToDeletePtr, ItemType& inorderSuccessor) {
    if (nodeToDeletePtr->getLeftChildPtr() == nullptr) {
        inorderSuccessor = nodeToDeletePtr -> getItem();
        return removeNode(nodeToDeletePtr);
    }

    nodeToDeletePtr->setLeftChildPtr(findSuccessor(nodeToDeletePtr->getLeftChildPtr(), inorderSuccessor));
    return nodeToDeletePtr;
}

//////////////////////////////////////////////////////////////
//      PUBLIC METHODS BEGIN HERE
//////////////////////////////////////////////////////////////

template<typename ItemType, typename KeyType>
BinarySearchTree<ItemType, KeyType>::BinarySearchTree() : rootPtr(nullptr){}

template<typename ItemType, typename KeyType>
BinarySearchTree<ItemType, KeyType>::BinarySearchTree(const BinarySearchTree<ItemType, KeyType>& tree) {
    rootPtr = clone(tree.rootPtr);
}

template<typename ItemType, typename KeyType>
BinarySearchTree<ItemType, KeyType>::~BinarySearchTree()
{
   this->destroyTree(rootPtr); // Call inherited method
}  // end destructor


//////////////////////////////////////////////////////////////
//      Public BinaryTreeInterface Methods Section - IMPLEMENT THESE
//////////////////////////////////////////////////////////////

template<typename ItemType, typename KeyType>
BinaryNode<ItemType>* BinarySearchTree<ItemType, KeyType>::clone(const BinaryNode<ItemType>* subTreePtr)
{
    BinaryNode<ItemType>* newTreePtr = nullptr;

    if(subTreePtr != nullptr) {
        newTreePtr = new BinaryNode<ItemType>(subTreePtr->getItem(), nullptr, nullptr);
        newTreePtr->setLeftChildPtr(clone(subTreePtr->getLeftChildPtr()));
        newTreePtr->setRightChildPtr(clone(subTreePtr->getRightChildPtr()));
    }

    return newTreePtr;
}

template<typename ItemType, typename KeyType>
bool BinarySearchTree<ItemType, KeyType>::add(const ItemType& newData)
{
    BinaryNode<ItemType>* newNode = new BinaryNode<ItemType>(newData);
    rootPtr = insertInorder(rootPtr, newNode);
    return true;
}

template<typename ItemType, typename KeyType>
void BinarySearchTree<ItemType, KeyType>::removeEntry(const KeyType& aKey) throw(NotFoundException) {
    bool isRemoved = false;

    rootPtr = removeHelper(rootPtr, aKey, isRemoved);

    if (!isRemoved)
        throw NotFoundException("Word not found\n\n");
}

template<typename ItemType, typename KeyType>
ItemType BinarySearchTree<ItemType, KeyType>::getEntry(const KeyType& aKey) const throw(NotFoundException)
{
    BinaryNode<ItemType>* keyPtr = findNode(rootPtr, aKey);

    if (keyPtr == nullptr) {
        throw NotFoundException("Word not found\n\n");
    }

    return keyPtr->getItem();
}

template<typename ItemType, typename KeyType>
bool BinarySearchTree<ItemType, KeyType>::contains(const KeyType& aKey) const
{
    if (findNode(rootPtr, aKey) != nullptr) {
        return true;
    }

    return false;
}

template<typename ItemType, typename KeyType>
void BinarySearchTree<ItemType, KeyType>::saveHelper(BinaryNode<ItemType>* subTreePtr, ofstream& outFile, Order order) {
    switch (order) {
        case IN_ORDER: {
            if (subTreePtr != nullptr) {
                saveHelper(rootPtr->getLeftChildPtr(), outFile, order);
                ItemType item = subTreePtr->getItem();
                outFile << item;
                saveHelper(subTreePtr->getRightChildPtr(), outFile, order);
            }
            break;
        }
        case PRE_ORDER: {
            if (subTreePtr != nullptr) {
                ItemType item = subTreePtr->getItem();
                outFile << item;
                saveHelper(subTreePtr->getLeftChildPtr(), outFile, order);
                saveHelper(subTreePtr->getRightChildPtr(), outFile, order);
            }
            break;
        }
        case POST_ORDER: {
            if (subTreePtr != nullptr) {
                saveHelper(subTreePtr->getLeftChildPtr(), outFile, order);
                saveHelper(subTreePtr->getRightChildPtr(), outFile, order);
                ItemType item = subTreePtr->getItem();
                outFile << item;
            }
            break;
        }
    }
}

template<typename ItemType, typename KeyType>
void BinarySearchTree<ItemType, KeyType>::save(ofstream& outFile, Order order) {
    saveHelper(rootPtr, outFile, order);
}


//////////////////////////////////////////////////////////////
//      Public Traversals Section - IMPLEMENT THESE
//////////////////////////////////////////////////////////////

template<typename ItemType, typename KeyType>
void BinarySearchTree<ItemType, KeyType>::inorderTraverse(void visit(ItemType&)) const
{
    inorderHelper(visit, rootPtr);
}

template<typename ItemType, typename KeyType>
void BinarySearchTree<ItemType, KeyType>::preorderTraverse(void visit(ItemType&)) const
{
    preorderHelper(visit, rootPtr);
}

template<typename ItemType, typename KeyType>
void BinarySearchTree<ItemType, KeyType>::postorderTraverse(void visit(ItemType&)) const
{
    postorderHelper(visit, rootPtr);
}

template<typename ItemType, typename KeyType>
void BinarySearchTree<ItemType, KeyType>::inorderHelper(void visit(ItemType&), BinaryNode<ItemType>* subTreePtr) const
{
    if (subTreePtr != nullptr) {
        inorderHelper(visit, subTreePtr->getLeftChildPtr());
        ItemType item = subTreePtr->getItem();
        visit(item);
        inorderHelper(visit, subTreePtr->getRightChildPtr());
    }
}

template<typename ItemType, typename KeyType>
void BinarySearchTree<ItemType, KeyType>::preorderHelper(void visit(ItemType&), BinaryNode<ItemType>* subTreePtr) const
{
    if (subTreePtr != nullptr) {
        ItemType item = subTreePtr->getItem();
        visit(item);
        preorderHelper(visit, subTreePtr->getLeftChildPtr());
        preorderHelper(visit, subTreePtr->getRightChildPtr());
    }
}

template<typename ItemType, typename KeyType>
void BinarySearchTree<ItemType, KeyType>::postorderHelper(void visit(ItemType&), BinaryNode<ItemType>* subTreePtr) const
{
    if (subTreePtr != nullptr) {
        postorderHelper(visit, subTreePtr->getLeftChildPtr());
        postorderHelper(visit, subTreePtr->getRightChildPtr());
        ItemType item = subTreePtr->getItem();
        visit(item);
    }
}
