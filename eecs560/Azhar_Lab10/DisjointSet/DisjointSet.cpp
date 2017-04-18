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
  SetNode<T> *setIPtr = nullptr;
  SetNode<T> *setJPtr = nullptr;

  for (int i = 0; i < numElements; i++) {
    if (auxillaryArr[i]->getKey() == setI) {
      setIPtr = auxillaryArr[i];
      break;
    }
  }

  for (int i = 0; i < numElements; i++) {
    if (auxillaryArr[i]->getKey() == setJ) {
      setJPtr = auxillaryArr[i];
      break;
    }
  }

  setIPtr = findHelper(setIPtr);
  setJPtr = findHelper(setJPtr);

  if (setIPtr != nullptr && setJPtr != nullptr) {
    if (setIPtr->getRank() > setJPtr->getRank()) {
      setJPtr->setParentPtr(setIPtr);
    } else {
      setIPtr->setParentPtr(setJPtr);
      if (setIPtr->getRank() == setJPtr->getRank()) {
        setJPtr->setRank(setJPtr->getRank() + 1);
      }
    }
  }
}

template<typename T>
SetNode<T> *DisjointSet<T>::findHelper(SetNode<T> *setPtr) {
  if (setPtr->getParentPtr() == nullptr) {
    return setPtr;
  }
  return findHelper(setPtr->getParentPtr());
}

template<typename T>
T DisjointSet<T>::find(T value) {
  SetNode<T> *elemPtr = nullptr;
  for (int i = 0; i < numElements; i++) {
    if (auxillaryArr[i]->getKey() == value) {
      elemPtr = auxillaryArr[i];
      break;
    }
  }

  if (elemPtr == nullptr) {
    return -1;
  }

  return findHelper(elemPtr)->getKey();
}
