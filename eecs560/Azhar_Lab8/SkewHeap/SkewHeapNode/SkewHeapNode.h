/**
*	@file   : SkewHeapNode.h
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Holds the node object for the skew heap
*/

#ifndef SKEW_HEAP_NODE_H
#define SKEW_HEAP_NODE_H

template<typename T>
class SkewHeapNode {
    private:
        T m_value;

        SkewHeapNode<T>* leftChildPtr;

        SkewHeapNode<T>* rightChildPtr;

    public:
        SkewHeapNode();

        SkewHeapNode(const T& val);

        SkewHeapNode(const T& val,
                     SkewHeapNode<T>* leftPtr,
                     SkewHeapNode<T>* rightPtr);

        void setValue(const T& val);

        T getValue() const;

        SkewHeapNode<T>* getLeftChildPtr() const;

        void setLeftChildPtr(SkewHeapNode<T>* leftPtr);

        SkewHeapNode<T>* getRightChildPtr() const;

        void setRightChildPtr(SkewHeapNode<T>* rightPtr);

};

#include "SkewHeapNode.cpp"

#endif
