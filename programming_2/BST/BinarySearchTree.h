/**
*	@file : BinarySearchTree.h
*	@author : Sharynne Azhar
*	@date : 2015.11.08
*	@brief: Header file for BST class
*/

#ifndef BINARYSEARCHTREE_H
#define BINARYSEARCHTREE_H

#include <iostream>

#include "BSTI.h"
#include "Node.h"

template <typename T>
class BinarySearchTree : public BSTI<T>
{
public:
    /*
    * @pre none
    * @post sets m_root to nullptr
    * @return none
    */
    BinarySearchTree();

    /*
    * @pre none
    * @post creates a deep copy of the other BST
    * @return none
    */
    BinarySearchTree(const BinarySearchTree<T>& other);

    /*
    * @pre assumes there is a valid tree
    * @post deletes the entire tree
    * @return none
    */
    ~BinarySearchTree();

    /*
    * @pre none
    * @post creates a deep copy of this
    * @return a pointer to a deep copy of this
    */
    BSTI<T>*clone();

    /*
    * @pre none
    * @post none
    * @return true if no nodes in the tree, false otherwise
    */
    bool isEmpty() const;

    /*
    * @pre none
    * @post adds the value to the tree
    * @return true if operation was successful
    */
    bool add(T value);

    /*
    * @pre the type T is comparable by the == operator
    * @post none
    * @return true if the value is in the tree, false otherwise
    */
    bool search(T value) const;

    /*
    * @pre none
    * @post the contents of the tree are printed to the console
    * @return none
    */
    void printTree(Order order) const;

    /*
    * @pre none
    * @post creates a vector of the tree
    * @return a vector with the contents of the tree in specified order
    */
    std::vector<T> treeToVector(Order order) const;

private:
    // pointer that is always looking that the root of the tree
    Node<T>* m_root;

    /*
    * @pre none
    * @post adds the value to the correct branch (left or right) of the tree
    * @return true if add operation was successful
    */
    bool add(T value, Node<T>* subTree);

    /*
    * @pre none
    * @post deletes the left subtree, right subtree, and root node
    * @return none
    */
    void deleteTree(Node<T>* subTree);

    /*
    * @pre none
    * @post none
    * @return true if the value is in the tree, false otherwise
    */
    bool search(T value, Node<T>* subtree) const;

    /*
    * @pre none
    * @post prints the tree in specified order
    * @return none
    */
    void printTree(Node<T>* subtree, Order order) const;

    /*
    * @pre none
    * @post loads the vector, vec, in the specified order
    * @return none
    */
    void treeToVector(Node<T>* subTree, Order order, std::vector<T>& vec) const;
};

#include "BinarySearchTree.hpp"
#endif
