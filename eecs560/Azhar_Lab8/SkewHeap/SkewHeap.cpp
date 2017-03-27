/**
*	@file   : SkewHeap.cpp
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Implementation of the min-leftist heap data structure
*/

template<typename T>
SkewHeap<T>::SkewHeap() : m_rootPtr(nullptr) {}

template<typename T>
SkewHeap<T>::~SkewHeap() {
  if (m_rootPtr) {
    deleteTree(m_rootPtr);
  }
}

template<typename T>
void SkewHeap<T>::deleteTree(SkewHeapNode<T>* subTreePtr) {
  if (subTreePtr->getLeftChildPtr()) {
    deleteTree(subTreePtr->getLeftChildPtr());
  }

  if (subTreePtr->getRightChildPtr()) {
    deleteTree(subTreePtr->getRightChildPtr());
  }

  delete subTreePtr;
}

template<typename T>
SkewHeapNode<T>* SkewHeap<T>::merge(SkewHeapNode<T>* leftTreePtr, SkewHeapNode<T>* rightTreePtr) {
  if (!leftTreePtr) {
    return rightTreePtr;
  } else if (!rightTreePtr) {
    return leftTreePtr;
  } else {
    if (leftTreePtr->getValue() > rightTreePtr->getValue()) {
      SkewHeapNode<T>* temp = leftTreePtr;
      leftTreePtr = rightTreePtr;
      rightTreePtr = temp;
      temp = nullptr;
    }

    SkewHeapNode<T>* tempPtr = leftTreePtr->getRightChildPtr();
    leftTreePtr->setRightChildPtr(leftTreePtr->getLeftChildPtr());
    leftTreePtr->setLeftChildPtr(merge(tempPtr, rightTreePtr));
  }

  return leftTreePtr;
}

template<typename T>
void SkewHeap<T>::insert(const T& val) {
  SkewHeapNode<T>* newNodePtr = new SkewHeapNode<T>(val);
  m_rootPtr = merge(m_rootPtr, newNodePtr);
}

template<typename T>
void SkewHeap<T>::deleteMin() {
  if (!m_rootPtr) {
    std::cout << "\nHeap is empty.\n";
  } else {
    SkewHeapNode<T>* nodeToDelete = m_rootPtr;
    SkewHeapNode<T>* leftChild = m_rootPtr->getLeftChildPtr();
    SkewHeapNode<T>* rightChild = m_rootPtr->getRightChildPtr();

    delete nodeToDelete;
    nodeToDelete = nullptr;

    m_rootPtr = merge(leftChild, rightChild);
  }
}

template<typename T>
void SkewHeap<T>::preorderHelper(SkewHeapNode<T>* subTreePtr) {
  if (subTreePtr) {
    std::cout << subTreePtr->getValue() << " ";
    preorderHelper(subTreePtr->getLeftChildPtr());
    preorderHelper(subTreePtr->getRightChildPtr());
  }
}

template<typename T>
void SkewHeap<T>::preorder() {
  std::cout << "\nPreorder: ";
  preorderHelper(m_rootPtr);
  std::cout << std::endl;
}

template<typename T>
void SkewHeap<T>::inorderHelper(SkewHeapNode<T>* subTreePtr) {
  if (subTreePtr) {
    inorderHelper(subTreePtr->getLeftChildPtr());
    std::cout << subTreePtr->getValue() << " ";
    inorderHelper(subTreePtr->getRightChildPtr());
  }
}

template<typename T>
void SkewHeap<T>::inorder() {
  std::cout << "\nInorder: ";
  inorderHelper(m_rootPtr);
  std::cout << std::endl;
}

template<typename T>
void SkewHeap<T>::levelorderHelper(SkewHeapNode<T>* subTreePtr) {
  if (!subTreePtr) {
    return;
  }

  Queue<SkewHeapNode<T>*> queue;
  queue.enqueue(subTreePtr);

  while (true) {
    int count = queue.getSize();
    if (count == 0) {
      return;
    }

    while (count > 0) {
      SkewHeapNode<T>* node = queue.peek();
      std::cout << node->getValue() << " ";

      if (node->getLeftChildPtr()) {
        queue.enqueue(node->getLeftChildPtr());
      }

      if (node->getRightChildPtr()) {
        queue.enqueue(node->getRightChildPtr());
      }

      queue.dequeue();

      count--;
    }

    std::cout << std::endl;
  }
}

template<typename T>
void SkewHeap<T>::levelorder() {
  std::cout << "\nLevel order:\n";
  levelorderHelper(m_rootPtr);
}
