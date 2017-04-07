/**
*	@file   : BinomialQueue.cpp
*	@author : Sharynne Azhar
*	@date   : 04-03-2017
* @desc   : Implementation of the binomial queue data structure
*/

template<typename T>
BinomialQueue<T>::BinomialQueue() {
  m_root = nullptr;
  for (int i = 0; i < MAX_NUM_TREES; i++) {
    queue[i] = nullptr;
  }
}

template<typename T>
BinomialQueue<T>::~BinomialQueue() {

}

template<typename T>
void BinomialQueue<T>::adjustTree() {
  // find the lowest order tree and set that as the root
  for (int i = 0; i < MAX_NUM_TREES && queue[i]; i++) {
    m_root = queue[i];
  }
}

template<typename T>
void BinomialQueue<T>::insert(const T& val) {
  BinomialQueueNode<T>* newNode = new BinomialQueueNode<T>(val);
  merge(newNode);
  adjustTree();
}

template<typename T>
void BinomialQueue<T>::merge(BinomialQueueNode<T>* newNode) {
  int order = newNode->getOrder();
  if (queue[order] == nullptr) {
    queue[order] = newNode;
  } else {
    merge(combine(newNode, queue[order]));
    queue[order] = nullptr;
  }
}

template<typename T>
BinomialQueueNode<T>* BinomialQueue<T>::combine(BinomialQueueNode<T>* q1, BinomialQueueNode<T>* q2) {
  if (q1->getValue() > q2->getValue()) {
    return combine(q2, q1);
  }

  if (q1->getFirstChildPtr() == nullptr) {
    q1->setFirstChildPtr(q2);
    q1->setOrder(q1->getOrder() + 1);
  } else {
    q2->setLeftSiblingPtr(q1->getFirstChildPtr()->getLeftSiblingPtr());
    q2->getLeftSiblingPtr()->setRightSiblingPtr(q2);
    q1->getFirstChildPtr()->setLeftSiblingPtr(q2);
    q1->setOrder(q1->getOrder() + 1);
  }

  return q1;
}

template<typename T>
void BinomialQueue<T>::deleteMin() {
  if (m_root == nullptr) {
    std::cout << "\nQueue is empty.\n";
    return;
  }

  BinomialQueueNode<T>* minNode = m_root;
  for (int i = 0; i < MAX_NUM_TREES; i++) {
    if (queue[i] != nullptr && queue[i]->getValue() < minNode->getValue()) {
      minNode = queue[i];
    }
  }

  int order = minNode->getOrder();
  BinomialQueueNode<T>** newQueue = new BinomialQueueNode<T>*[order];
  BinomialQueueNode<T>* firstChild = minNode->getFirstChildPtr();
  for (int i = 0; firstChild != nullptr; i++) {
    newQueue[i] = firstChild;
    firstChild = firstChild->getRightSiblingPtr();
  }

  delete minNode;
  queue[order] = nullptr;

  for (int i = 0; i < order; i++) {
    BinomialQueueNode<T>* node = newQueue[i];
    node->setLeftSiblingPtr(node);
    node->setRightSiblingPtr(nullptr);
    merge(node);
  }

  delete [] newQueue;
}

template<typename T>
void BinomialQueue<T>::levelorder() {
  std::cout << "\nLevelorder:\n\n";
  for (int i = 0; i < MAX_NUM_TREES; i++) {
    if (queue[i] != nullptr) {
      levelorderHelper(queue[i]);
      std::cout << "---" << std::endl;
    }
  }
}

template<typename T>
void BinomialQueue<T>::levelorderHelper(BinomialQueueNode<T>* ptr) {
  Queue<BinomialQueueNode<T>*> levelorderQueue1;
  Queue<BinomialQueueNode<T>*> levelorderQueue2;

  levelorderQueue1.enqueue(ptr);
  while (!(levelorderQueue1.isEmpty())) {
    BinomialQueueNode<T>* nodeToVisitPtr = levelorderQueue1.peek();
    levelorderQueue1.dequeue();

    if (nodeToVisitPtr != nullptr) {
      if (nodeToVisitPtr->getFirstChildPtr() != nullptr) {
        BinomialQueueNode<T>* childPtr = nodeToVisitPtr->getFirstChildPtr();
        do {
          levelorderQueue2.enqueue(childPtr);
          childPtr = childPtr->getRightSiblingPtr();
        } while (childPtr != nullptr);
      }
      std::cout << nodeToVisitPtr->getValue() << " ";
    }

    if (levelorderQueue1.isEmpty()) {
      std::cout << std::endl;
      while(!(levelorderQueue2.isEmpty())) {
        levelorderQueue1.enqueue(levelorderQueue2.peek());
        levelorderQueue2.dequeue();
      }
    }
  }
}
