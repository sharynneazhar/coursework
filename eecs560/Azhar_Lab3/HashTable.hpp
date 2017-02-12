/**
*	@file : HashTable.hpp
*	@author : Sharynne Azhar
*	@date : 02-03-2017
*/

template <typename T>
HashTable<T>::HashTable(T tableSize, char hashMethod) {
  m_tableSize = tableSize;
  m_hashMethod = hashMethod;
  hashTable = new HashObject<T>[tableSize];
  for (int i = 0; i < m_tableSize; i++) {
    hashTable[i] = HashObject<T>();
  }
}

template <typename T>
HashTable<T>::~HashTable() {
  delete [] hashTable;
}

template <typename T>
int HashTable<T>::hash(T value, int iteration) {
  if (m_hashMethod == 'Q')
    return (((value % m_tableSize) + (iteration * iteration)) % m_tableSize);
  return value;
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

  std::cout << std::endl;
  std::cout << std::left << std::setw(30) << std::setfill('-') << "-";
  std::cout << "\nHash Method: " << method << std::endl;
  std::cout << std::left << std::setw(30) << std::setfill('-') << "-";
  std::cout << std::endl;

  for (int i = 0; i < m_tableSize; i++) {
    std::string flagStr = hashTable[i].getFlag() ? " flag = true" : " flag = false";
    std::cout << std::left << std::setfill(' ')
              << std::setw(2) << i << "  |  "
              << std::setw(2) << hashTable[i].getValue() << "  |  "
              << std::setw(15) << flagStr
              << std::endl;
  }

  std::cout << std::left << std::setw(30) << std::setfill('-') << "-";
  std::cout << std::endl;
}
