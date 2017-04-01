/**
*	@file   : LeftistHeap.cpp
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Implementation of the min-leftist heap data structure
*/

template<typename T>
LeftistHeap<T>::LeftistHeap() : m_rootPtr(nullptr) {}

template<typename T>
LeftistHeap<T>::~LeftistHeap() {
  if (m_rootPtr) {
    deleteTree(m_rootPtr);
  }
}

template<typename T>
void LeftistHeap<T>::deleteTree(LeftistHeapNode<T>* subTreePtr) {
  if (subTreePtr->getLeftChildPtr()) {
    deleteTree(subTreePtr->getLeftChildPtr());
  }

  if (subTreePtr->getRightChildPtr()) {
    deleteTree(subTreePtr->getRightChildPtr());
  }

  delete subTreePtr;
}

template<typename T>
LeftistHeapNode<T>* LeftistHeap<T>::merge(LeftistHeapNode<T>* leftTreePtr, LeftistHeapNode<T>* rightTreePtr) {
  if (!leftTreePtr) {
    return rightTreePtr;
  } else if (!rightTreePtr) {
    return leftTreePtr;
  } else {
    if (leftTreePtr->getValue() > rightTreePtr->getValue()) {
      LeftistHeapNode<T>* temp = leftTreePtr;
      leftTreePtr = rightTreePtr;
      rightTreePtr = temp;
      temp = nullptr;
    }

    LeftistHeapNode<T>* tempPtr = merge(leftTreePtr->getRightChildPtr(), rightTreePtr);
    leftTreePtr->setRightChildPtr(tempPtr);

    if (!leftTreePtr->getLeftChildPtr() || (leftTreePtr->getLeftChildPtr()->getRank() < leftTreePtr->getRightChildPtr()->getRank())) {
      LeftistHeapNode<T>* temp = leftTreePtr->getLeftChildPtr();
      leftTreePtr->setLeftChildPtr(leftTreePtr->getRightChildPtr());
      leftTreePtr->setRightChildPtr(temp);
      temp = nullptr;
    }

    if (!leftTreePtr->getRightChildPtr()) {
      leftTreePtr->setRank(1);
    } else {
      leftTreePtr->setRank(leftTreePtr->getRightChildPtr()->getRank() + 1);
    }
  }

  return leftTreePtr;
}

template<typename T>
void LeftistHeap<T>::insert(const T& val) {
  LeftistHeapNode<T>* newNodePtr = new LeftistHeapNode<T>(val);
  m_rootPtr = merge(m_rootPtr, newNodePtr);
}

template<typename T>
void LeftistHeap<T>::deleteMin() {
  if (!m_rootPtr) {
    std::cout << "\nHeap is empty.\n";
  } else {
    LeftistHeapNode<T>* nodeToDelete = m_rootPtr;
    LeftistHeapNode<T>* leftChild = m_rootPtr->getLeftChildPtr();
    LeftistHeapNode<T>* rightChild = m_rootPtr->getRightChildPtr();

    delete nodeToDelete;
    nodeToDelete = nullptr;

    m_rootPtr = merge(leftChild, rightChild);
  }
}

template<typename T>
void LeftistHeap<T>::preorderHelper(LeftistHeapNode<T>* subTreePtr) {
  if (subTreePtr) {
    std::cout << subTreePtr->getValue() << " ";
    preorderHelper(subTreePtr->getLeftChildPtr());
    preorderHelper(subTreePtr->getRightChildPtr());
  }
}

template<typename T>
void LeftistHeap<T>::preorder() {
  std::cout << "\nPreorder: ";
  preorderHelper(m_rootPtr);
  std::cout << std::endl;
}

template<typename T>
void LeftistHeap<T>::inorderHelper(LeftistHeapNode<T>* subTreePtr) {
  if (subTreePtr) {
    inorderHelper(subTreePtr->getLeftChildPtr());
    std::cout << subTreePtr->getValue() << " ";
    inorderHelper(subTreePtr->getRightChildPtr());
  }
}

template<typename T>
void LeftistHeap<T>::inorder() {
  std::cout << "\nInorder: ";
  inorderHelper(m_rootPtr);
  std::cout << std::endl;
}

template<typename T>
void LeftistHeap<T>::levelorderHelper(LeftistHeapNode<T>* subTreePtr) {
  if (!subTreePtr) {
    return;
  }

  Queue<LeftistHeapNode<T>*> queue;
  queue.enqueue(subTreePtr);

  while (true) {
    int count = queue.getSize();
    if (count == 0) {
      return;
    }

    while (count > 0) {
      LeftistHeapNode<T>* node = queue.peek();
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
void LeftistHeap<T>::levelorder() {
  std::cout << "\nLevel order:\n";
  levelorderHelper(m_rootPtr);
}
