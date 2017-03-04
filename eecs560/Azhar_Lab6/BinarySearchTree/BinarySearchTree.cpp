/**
*	@file   : BinarySearchTree.cpp
*	@author : Sharynne Azhar
*	@date   : 02-27-2017
* @desc   : Implementation of BinarySearchTree class
*/

template<typename T>
BinarySearchTree<T>::BinarySearchTree() : rootPtr(nullptr) {}

template<typename T>
BinarySearchTree<T>::~BinarySearchTree() {
  destroyTree(rootPtr);
}

template<typename T>
void BinarySearchTree<T>::destroyTree(BinaryNode<T>* subTreePtr) {
  if (!subTreePtr) {
    destroyTree(subTreePtr->getLeftChildPtr());
    destroyTree(subTreePtr->getRightChildPtr());
  }
  delete subTreePtr;
}

template<typename T>
void BinarySearchTree<T>::insertItem(const T& item) { // leaky, but weird behavior if delete newNode
  BinaryNode<T>* newNode = new BinaryNode<T>(item);
  rootPtr = insertInorder(rootPtr, newNode);
}

template<typename T>
BinaryNode<T>* BinarySearchTree<T>::insertInorder(BinaryNode<T>* subTreePtr, BinaryNode<T>* newNodePtr) {
  if (!subTreePtr) {
    return newNodePtr;
  } else if (subTreePtr->getItem() == newNodePtr->getItem()){
    return subTreePtr;
  } else if (subTreePtr->getItem() > newNodePtr->getItem()) {
    BinaryNode<T>* tempPtr = insertInorder(subTreePtr->getLeftChildPtr(), newNodePtr);
    subTreePtr->setLeftChildPtr(tempPtr);
  } else {
    BinaryNode<T>* tempPtr = insertInorder(subTreePtr->getRightChildPtr(), newNodePtr);
    subTreePtr->setRightChildPtr(tempPtr);
  }
  return subTreePtr;
}

template<typename T>
void BinarySearchTree<T>::removeItem(const T& item) {
  bool isRemoved = false;
  rootPtr = removeItem(rootPtr, item, isRemoved);
}

template<typename T>
BinaryNode<T>* BinarySearchTree<T>::removeItem(BinaryNode<T>* subTreePtr, const T item, bool& isRemoved) {
  if (!subTreePtr) {
    std::cout << "\nElement " << item << " is not in the tree.\n";
    isRemoved = false;
    return nullptr;
  } else if (subTreePtr->getItem() == item) {
    subTreePtr = removeNode(subTreePtr);
    isRemoved = true;
  } else if (subTreePtr->getItem() > item) {
    BinaryNode<T>* tempPtr = removeItem(subTreePtr->getLeftChildPtr(), item, isRemoved);
    subTreePtr->setLeftChildPtr(tempPtr);
  } else {
    BinaryNode<T>* tempPtr = removeItem(subTreePtr->getRightChildPtr(), item, isRemoved);
    subTreePtr->setRightChildPtr(tempPtr);
  }
  return subTreePtr;
}

template<typename T>
BinaryNode<T>* BinarySearchTree<T>::removeNode(BinaryNode<T>* nodeToDelete) {
  if (nodeToDelete->isLeaf()) {
    delete nodeToDelete;
    nodeToDelete = nullptr;
    return nodeToDelete;
  } else if (nodeToDelete->isOnlyChild()) {
    BinaryNode<T>* nodeToConnect;
    if (nodeToDelete->getLeftChildPtr()) {
      nodeToConnect = nodeToDelete->getLeftChildPtr();
    } else {
      nodeToConnect = nodeToDelete->getRightChildPtr();
    }
    delete nodeToDelete;
    nodeToDelete = nullptr;
    return nodeToConnect;
  } else {
    T inorderSuccessor;
    BinaryNode<T>* tempPtr = removeMinInRightSubTree(nodeToDelete->getRightChildPtr(), inorderSuccessor);
    nodeToDelete->setRightChildPtr(tempPtr);
    nodeToDelete->setItem(inorderSuccessor);
  }
  return nodeToDelete;
}

template<typename T>
BinaryNode<T>* BinarySearchTree<T>::removeMinInRightSubTree(BinaryNode<T>* nodeToDelete, T& inorderSuccessor) {
  if (!nodeToDelete->getLeftChildPtr()) {
    inorderSuccessor = nodeToDelete->getItem();
    return removeNode(nodeToDelete);
  }

  nodeToDelete->setLeftChildPtr(removeMinInRightSubTree(nodeToDelete->getLeftChildPtr(), inorderSuccessor));
  return nodeToDelete;
}

template<typename T>
BinaryNode<T>* BinarySearchTree<T>::search(const T& item) {
  BinaryNode<T>* nodePtr = searchHelper(rootPtr, item);
  return nodePtr;
}

template<typename T>
BinaryNode<T>* BinarySearchTree<T>::searchHelper(BinaryNode<T>* subTreePtr, const T& item) const {
  if (!subTreePtr) {
    return nullptr;
  } else if (subTreePtr->getItem() == item) {
    return subTreePtr;
  } else if (subTreePtr->getItem() > item) {
    return searchHelper(subTreePtr->getLeftChildPtr(), item);
  }
  return searchHelper(subTreePtr->getRightChildPtr(), item);
}

template<typename T>
void BinarySearchTree<T>::deleteMin() {
  bool isRemoved = false;
  if (rootPtr) {
    T minItem = findMin(rootPtr);
    rootPtr = removeItem(rootPtr, minItem, isRemoved);
  }
}

template<typename T>
T BinarySearchTree<T>::findMin(BinaryNode<T>* subTreePtr) {
  if (!subTreePtr->getLeftChildPtr()) {
    return subTreePtr->getItem();
  }
  return findMin(subTreePtr->getLeftChildPtr());
}

template<typename T>
void BinarySearchTree<T>::deleteMax() {
  bool isRemoved = false;
  if (rootPtr) {
    T maxItem = findMax(rootPtr);
    rootPtr = removeItem(rootPtr, maxItem, isRemoved);
  }
}

template<typename T>
T BinarySearchTree<T>::findMax(BinaryNode<T>* subTreePtr) {
  if (!subTreePtr->getRightChildPtr()) {
    return subTreePtr->getItem();
  }
  return findMax(subTreePtr->getRightChildPtr());
}

template<typename T>
void BinarySearchTree<T>::preorder() const {
  std::cout << "\nPreorder: ";
  preorderHelper(rootPtr);
  std::cout << std::endl;
}

template<typename T>
void BinarySearchTree<T>::preorderHelper(BinaryNode<T>* subTreePtr) const {
  if (subTreePtr) {
    std::cout << subTreePtr->getItem() << " ";
    preorderHelper(subTreePtr->getLeftChildPtr());
    preorderHelper(subTreePtr->getRightChildPtr());
  }
}

template<typename T>
void BinarySearchTree<T>::inorder() const {
  std::cout << "\nInorder: ";
  inorderHelper(rootPtr);
  std::cout << std::endl;
}

template<typename T>
void BinarySearchTree<T>::inorderHelper(BinaryNode<T>* subTreePtr) const {
  if (subTreePtr) {
    inorderHelper(subTreePtr->getLeftChildPtr());
    std::cout << subTreePtr->getItem() << " ";
    inorderHelper(subTreePtr->getRightChildPtr());
  }
}

template<typename T>
void BinarySearchTree<T>::levelorder() const {
  std::cout << "\nLevelorder: ";

  Queue<BinaryNode<T>*> queue;

  if (rootPtr) {
    BinaryNode<T>* nodeToVisit;
    queue.enqueue(rootPtr);

    do {
      nodeToVisit = queue.peek();
      std::cout << nodeToVisit->getItem() << " ";

      if (nodeToVisit->getLeftChildPtr()) {
        queue.enqueue(nodeToVisit->getLeftChildPtr());
      }

      if (nodeToVisit->getRightChildPtr()) {
        queue.enqueue(nodeToVisit->getRightChildPtr());
      }

      queue.dequeue();
    } while(!queue.isEmpty());
  }

  std::cout << std::endl;
}
