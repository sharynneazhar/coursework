/**
*	@file : HashTable.hpp
*	@author : Sharynne Azhar
*	@date : 02-12-2017
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
int HashTable<T>::hash(const T value, int iteration) {
  // quadratic hashing
  if (m_hashMethod == 'Q')
    return (((value % m_tableSize) + (iteration * iteration)) % m_tableSize);

  // double hashing
  int p = 5;
  return ((value % m_tableSize) + (iteration * (p - (value % p)))) % m_tableSize;
}

template <typename T>
int HashTable<T>::findPosition(const T value) {
  int key, iter = 0;
  do {
    key = hash(value, iter);
    if (hashTable[key].getValue() == value)
      return key;
    iter++;
  } while ((hashTable[key].insertable()) && (iter < m_tableSize));
  return -1;
}

template <typename T>
bool HashTable<T>::contains(const T value) {
  int pos = findPosition(value);
  return (pos != -1);
}

template <typename T>
void HashTable<T>::insertValue(const T value) {
  if (contains(value)) {
    std::cout << "\nValue already exists.\n";
    return;
  }

  int key, iter = 0;
  do {
    key = hash(value, iter);
    if (hashTable[key].insertable()) {
      hashTable[key].setValue(value);
      return;
    }
    iter++;
  } while (iter < m_tableSize);

  std::cout << "\nUnable to insert value into table. Skipping...\n";
}

template <typename T>
void HashTable<T>::deleteValue(const T value) {
  if (!contains(value)) {
    std::cout << "\nValue not found.\n";
    return;
  }

  int key = findPosition(value);
  hashTable[key].setValue(-1);
  hashTable[key].setFlag(true);
}

template <typename T>
void HashTable<T>::printList() {
  std::string method = m_hashMethod == 'Q' ? "Quadratic" : "Double Hashing";

  std::cout << std::endl;
  std::cout << std::left << std::setw(30) << std::setfill('-') << "-";
  std::cout << "\nHash Method: " << method << std::endl;
  std::cout << std::left << std::setw(30) << std::setfill('-') << "-" << std::endl;

  std::cout << std::left << std::setw(6) << std::setfill(' ') << "key";
  std::cout << std::left << std::setw(8) << std::setfill(' ') << "value";
  std::cout << std::left << std::setw(15) << std::setfill(' ') << "flag" << std::endl;
  std::cout << std::left << std::setw(30) << std::setfill('-') << "-" << std::endl;

  for (int i = 0; i < m_tableSize; i++) {
    std::string flagStr = hashTable[i].getFlag() ? "true" : "false";
    std::cout << std::left << std::setw(6) << std::setfill(' ') << i;
    std::cout << std::left << std::setw(8) << std::setfill(' ') << hashTable[i].getValue();
    std::cout << std::left << std::setw(15) << std::setfill(' ') << flagStr << std::endl;
  }

  std::cout << std::left << std::setw(30) << std::setfill('-') << "-" << std::endl;
}
