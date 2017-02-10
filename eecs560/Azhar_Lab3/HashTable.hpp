/**
*	@file : HashTable.hpp
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

template <typename T>
HashTable<T>::HashTable(T tableSize, char hashMethod) {
  m_hashMethod = hashMethod;
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
  delete [] hashTable;
}

template <typename T>
int HashTable<T>::hash(T value) {
  return (value % m_tableSize);
}

template <typename T>
bool HashTable<T>::find(T value) {
  int key = hash(value);
  Node<T>* currNode = hashTable[key];
  while (currNode) {
    if (currNode->getValue() == value)
      return true;
    currNode = currNode->getNext();
  }
  return false;
}

template <typename T>
void HashTable<T>::insertValue(T value) {
  if (find(value)) {
    return;
  }

  int key = hash(value);
  Node<T>* newNode = new Node<T>(value);

  if (!hashTable[key]) {
    hashTable[key] = newNode;
  } else {
    newNode->setNext(hashTable[key]);
    hashTable[key]->setPrev(newNode);
    hashTable[key] = newNode;
  };
}

template <typename T>
void HashTable<T>::deleteValue(T value) {
  if (!find(value)) {
    std::cout << "\nValue is not in list.\n";
    return;
  }

  int key = hash(value);
  Node<T>* currNode = hashTable[key];

  while (currNode) {
    Node<T>* nextNode = currNode->getNext();
    if (currNode->getValue() == value) {
      if (!currNode->getPrev()) {
        hashTable[key] = currNode->getNext();
      } else {
        currNode->getPrev()->setNext(currNode->getNext());
      }
      delete currNode;
    }
    currNode = nextNode;
  }
}

template <typename T>
void HashTable<T>::printList() {
  std::string method = m_hashMethod == 'Q' ? "Quadratic" : "Double Hashing";
  std::cout << "\nHash Method: " << method << std::endl << std::endl;

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
