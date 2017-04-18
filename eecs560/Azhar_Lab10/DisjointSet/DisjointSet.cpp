/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 17 April 2017
 Description   : Class implementation for disjoint set
 ============================================================================
*/

template<typename T>
DisjointSet<T>::DisjointSet() {}

template<typename T>
DisjointSet<T>::DisjointSet(T *set, int size) {
  numElements = size;
  auxillaryArr = new SetNode<T>*[size];
  for (int i = 0; i < size; i++) {
    auxillaryArr[i] = new SetNode<T>(set[i]);
  }
}

template<typename T> DisjointSet<T>::~DisjointSet() {
  delete [] auxillaryArr;
}

template<typename T>
void DisjointSet<T>::setUnion(const T setI, const T setJ) {
  SetNode<T> *setITree = nullptr;
  SetNode<T> *setJTree = nullptr;

  // find matching node for set Si in auxillary array
  for (int i = 0; i < numElements; i++) {
    if (auxillaryArr[i]->getKey() == setI) {
      setITree = auxillaryArr[i];
      break;
    }
  }

  // find matching node for set Sj in auxillary array
  for (int i = 0; i < numElements; i++) {
    if (auxillaryArr[i]->getKey() == setJ) {
      setJTree = auxillaryArr[i];
      break;
    }
  }

  // find the root of each tree
  setITree = findRoot(setITree);
  setJTree = findRoot(setJTree);

  // union the sets
  if (setITree != nullptr && setJTree != nullptr) {
    if (setITree->getRank() > setJTree->getRank()) {
      setJTree->setParentPtr(setITree);
    } else {
      setITree->setParentPtr(setJTree);
      if (setITree->getRank() == setJTree->getRank()) {
        setJTree->setRank(setJTree->getRank() + 1);
      }
    }
  }
}

template<typename T>
SetNode<T> *DisjointSet<T>::findRoot(SetNode<T> *setPtr) {
  if (setPtr->getParentPtr() == nullptr) {
    return setPtr;
  }
  return findRoot(setPtr->getParentPtr());
}

template<typename T>
T DisjointSet<T>::find(T value) {
  SetNode<T> *elemPtr = nullptr;

  // find matching node in auxillary array
  for (int i = 0; i < numElements; i++) {
    if (auxillaryArr[i]->getKey() == value) {
      elemPtr = auxillaryArr[i];
      break;
    }
  }

  // value not found in tree
  if (elemPtr == nullptr) {
    return -1;
  }

  // return the root of the tree containing the value
  return findRoot(elemPtr)->getKey();
}
