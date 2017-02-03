/**
*	@file : OpenHasher.hpp
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

template <typename T>
OpenHasher<T>::OpenHasher() {
  m_tableSize = 0;
  hashTable = new Node<T>*[0];
}

template <typename T>
OpenHasher<T>::OpenHasher(T tableSize) {
  m_tableSize = tableSize;
  hashTable = new Node<T>*[tableSize];
  for (int i = 0; i < m_tableSize; i++) {
    hashTable[i] = nullptr;
  }
}


template <typename T>
OpenHasher<T>::~OpenHasher() {
  for (int i = 0; i < m_tableSize; i++) {
    Node<T>* currNode = hashTable[i];
    while (currNode) {
      Node<T>* nodeToDelete = currNode;
      currNode = currNode->getNext();
      delete nodeToDelete;
    }
  }
}

template <typename T>
int OpenHasher<T>::hash(T value) {
  return (value % m_tableSize);
}

template <typename T>
bool OpenHasher<T>::find(T value) {
  int hashKey = hash(value);
  Node<T>* currNode = hashTable[hashKey];
  while (currNode) {
    if (currNode->getValue() == value)
      return true;
    currNode = currNode->getNext();
  }
  return false;
}

template <typename T>
void OpenHasher<T>::insertValue(T value) {
  if (find(value))
    return;

  
}

template <typename T>
void OpenHasher<T>::deleteValue(T value) {
  // if (!deleteHelper(m_front, value))
  //   std::cout << "\nValue not found.\n";
}

template <typename T>
void OpenHasher<T>::printList() {
  for (int i = 0; i < m_tableSize; i++) {
    std::cout << i << ": ";
    Node<T>* currNode = hashTable[i];
    while (currNode) {
      std::cout << currNode->getValue() << " ";
      currNode = currNode->getNext();
    }
    std::cout << std::endl;
  }
}
