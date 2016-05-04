//  Subsetted from:
//  Created by Frank M. Carrano and Tim Henry.
//  Copyright (c) 2013 __Pearson Education__. All rights reserved.

// Listing 16-4.

/** Link-based implementation of the ADT binary search tree.
 @file BinarySearchTree.h */

#ifndef _BINARY_SEARCH_TREE
#define _BINARY_SEARCH_TREE

#include "BinaryNode.h"
#include "NotFoundException.h"

enum Order
{
    IN_ORDER,
    PRE_ORDER,
    POST_ORDER
};

template<typename ItemType, typename KeyType>
class BinarySearchTree
{
    private:
       BinaryNode<ItemType>* rootPtr;

       // Recursively deletes all nodes from the tree.
       void destroyTree(BinaryNode<ItemType>* subTreePtr);

       // Recursively finds where the given node should be placed and
       // inserts it in a leaf at that point.
       BinaryNode<ItemType>* insertInorder(BinaryNode<ItemType>* subTreePtr, BinaryNode<ItemType>* newNode);

       // Returns a pointer to the node containing the given value,
       // or nullptr if not found.
       BinaryNode<ItemType>* findNode(BinaryNode<ItemType>* treePtr, const KeyType& target) const;


       BinaryNode<ItemType>* removeHelper(BinaryNode<ItemType>* subTreePtr, const KeyType target, bool& success);
       BinaryNode<ItemType>* removeNode(BinaryNode<ItemType>* nodeToDeletePtr);
       BinaryNode<ItemType>* findSuccessor(BinaryNode<ItemType>* nodeToDeletePtr, ItemType& inorderSucessor);


       void saveHelper(BinaryNode<ItemType>* subTreePtr, ofstream& outFile, Order order);

       void inorderHelper(void visit(ItemType&), BinaryNode<ItemType>* subTreePtr) const;
       void preorderHelper(void visit(ItemType&), BinaryNode<ItemType>* subTreePtr) const;
       void postorderHelper(void visit(ItemType&), BinaryNode<ItemType>* subTreePtr) const;


    public:
       //------------------------------------------------------------
       // Constructor and Destructor Section.
       //------------------------------------------------------------
       BinarySearchTree();
       BinarySearchTree(const BinarySearchTree<ItemType, KeyType>& tree);
       virtual ~BinarySearchTree();

       //------------------------------------------------------------
       // Public Methods Section.
       //------------------------------------------------------------
       bool add(const ItemType& newEntry);
       void removeEntry(const KeyType& aKey) throw(NotFoundException);

       bool contains(const KeyType& aKey) const;
       ItemType getEntry(const KeyType& aKey) const throw(NotFoundException);
       BinaryNode<ItemType>* clone(const BinaryNode<ItemType>* subTreePtr);

       void save(ofstream& outFile, Order order);

       //------------------------------------------------------------
       // Public Traversals Section.
       //------------------------------------------------------------
       void inorderTraverse(void visit(ItemType&)) const;
       void preorderTraverse(void visit(ItemType&)) const;
       void postorderTraverse(void visit(ItemType&)) const;

}; // end BinarySearchTree

#include "BinarySearchTree.cpp"

#endif
