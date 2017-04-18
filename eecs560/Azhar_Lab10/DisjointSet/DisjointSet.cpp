/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 17 April 2017
 Description   : Class implementation for disjoint set
 ============================================================================
*/

DisjointSet::DisjointSet() {}

DisjointSet::DisjointSet(int *set, int size) {
  numElements = size;
  auxillaryArr = new SetNode*[size];
  for (int i = 0; i < size; i++) {
    auxillaryArr[i] = new SetNode(set[i]);
  }
}

 DisjointSet::~DisjointSet() {
  delete [] auxillaryArr;
}

void DisjointSet::setUnion(const int setI, const int setJ) {
  SetNode* setITree = nullptr;
  SetNode* setJTree = nullptr;

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

SetNode* DisjointSet::findRoot(SetNode *setPtr) {
  if (setPtr->getParentPtr() == nullptr) {
    return setPtr;
  }
  return findRoot(setPtr->getParentPtr());
}

int DisjointSet::find(int value) {
  SetNode *elemPtr = nullptr;

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
