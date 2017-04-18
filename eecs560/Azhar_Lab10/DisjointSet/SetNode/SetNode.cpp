/*
 ============================================================================
 Author        : Sharynne Azhar
 Date          : 17 April 2017
 Description   : Class implementation for disjoint set node
 ============================================================================
*/

template<typename T>
SetNode<T>::SetNode() : rank(0), parentPtr(nullptr) {}

template<typename T>
SetNode<T>::SetNode(const T &k) : rank(0), parentPtr(nullptr) {
  key = k;
}

template<typename T> T SetNode<T>::getKey() const {
  return key;
}

template<typename T>
void SetNode<T>::setKey(const T &k) {
  key = k;
}

template<typename T> int SetNode<T>::getRank() const {
  return rank;
}

template<typename T> void SetNode<T>::setRank(const int r) {
  rank = r;
}

template<typename T>
SetNode<T> *SetNode<T>::getParentPtr() const {
  return parentPtr;
}

template<typename T>
void SetNode<T>::setParentPtr(SetNode<T> *nodePtr) {
  parentPtr = nodePtr;
}
