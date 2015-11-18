/**
*	@file : BSTI.h
*	@author : Sharynne Azhar
*	@date : 2015.11.08
*	@brief: Header file for BST Interface
*/

#ifndef BSTI_H
#define BSTI_H

#include <vector>

enum Order
{
    PRE_ORDER,
    IN_ORDER,
    POST_ORDER
};

template <typename T>
class BSTI
{
public:
    /*
    * @pre assumes a BSTI exists
    * @post deletes the entire tree
    * @return none
    */
    virtual ~BSTI(){};

    /*
    * @pre this is in a valid state
    * @post creates a deep copy of this
    * @return a pointer to a deep copy of this
    */
    virtual BSTI<T>* clone() = 0;

    /*
    * @pre none
    * @post none
    * @return true if no nodes in the tree, false otherwise
    */
    virtual bool isEmpty() const = 0;

    /*
    * @pre value is a valid T
    * @post a new Node<T> is created and inserted into the tree base
    * @return false if the value couldn't be added
    */
    virtual bool add(T value) = 0;

    /*
    * @pre the type T is comparable by the == operator
    * @post none
    * @return true if the value is in the tree, false otherwise
    */
    virtual bool search(T value) const = 0;

    /*
    * @pre none
    * @post contents printed to the console in specified order
    * @return none
    */
    virtual void printTree(Order order) const = 0;

    /*
    * @pre none
    * @post none
    * @return a vector with the contents of the tree in the specified order
    */
    virtual std::vector<T> treeToVector(Order order) const = 0;
};

#endif
