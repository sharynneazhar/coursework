/**
*	@file : HashTable.hpp
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

template <typename T>
HashTable<T>::HashTable(T tableSize) {
  m_tableSize = tableSize;
  hashTable = new Node<T>*[tableSize];
  for (int i = 0; i < m_tableSize; i++) {
    hashTable[i] = nullptr;
  }
}

template <typename T>
HashTable<T>::~HashTable() {
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
int HashTable<T>::hash(T value) {
  return (value % m_tableSize);
}

template <typename T>
bool HashTable<T>::find(T value) {
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
void HashTable<T>::insertValue(T value) {
  if (!find(value)) {
    Node<T>* newNode = new Node<T>(value);
    int key = hash(value);
    if (hashTable[key] == nullptr) {
      hashTable[key] = newNode;
    } else {
      newNode->setNext(hashTable[key]);
      hashTable[key] = newNode;
    };
  }
}

template <typename T>
void HashTable<T>::deleteValue(T value) {
  // if (!deleteHelper(m_front, value))
  //   std::cout << "\nValue not found.\n";
}

template <typename T>
void HashTable<T>::printList() {
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
