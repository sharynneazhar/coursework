/**
*	@file   : MinHeap.cpp
*	@author : Sharynne Azhar
*	@date   : 03-02-2017
* @desc   : Implementation file for the min 3-heap class
*/

template<typename T>
MinHeap<T>::MinHeap() {
  m_k = 3;
  m_maxHeapSize = 200;
  m_numEntries = 0;
  m_heapArr = new T[m_maxHeapSize];
  for (int i = 0; i < m_maxHeapSize; i++) {
    m_heapArr[i] = -1;
  }
}

template<typename T>
MinHeap<T>::MinHeap(const T k, const int size) {
  m_k = k;
  m_maxHeapSize = size;
  m_numEntries = 0;
  m_heapArr = new T[size];
  for (int i = 0; i < m_maxHeapSize; i++) {
    m_heapArr[i] = -1;
  }
}

template<typename T>
MinHeap<T>::MinHeap(const T k, const int size, const std::string fileName) {
  std::ifstream file;
  file.open(fileName);

  if (file.fail()) {
    std::cout << "\n>> ERROR: Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  m_k = k;
  m_maxHeapSize = size;
  m_numEntries = 0;
  m_heapArr = new T[size];

  // read data from file
  int value, idx = 0;
  while (file >> value) {
    m_heapArr[idx] = value;
    idx++;
    m_numEntries++;
  }

  file.close();

  // fill empty slots with -1
  for (int i = m_numEntries; i < m_maxHeapSize; i++) {
    m_heapArr[i] = -1;
  }

  buildHeap();
}

template<typename T>
MinHeap<T>::~MinHeap() {
  delete [] m_heapArr;
}

template<typename T>
int MinHeap<T>::getParentIndex(int childIndex) const {
  if (childIndex == 0)
    return -1;
  return floor((childIndex - 1) / m_k);
}

template<typename T>
int MinHeap<T>::getChildIndex(int parentIndex, int childNum) const {
  int child = (m_k * parentIndex) + childNum;
  if (child >= m_maxHeapSize)
    return -1;
  return child;
}

template<typename T>
void MinHeap<T>::buildHeap() {
  int firstNonLeafIndex = getParentIndex(m_numEntries - 1);
  for (int i = firstNonLeafIndex; i >= 0; i--) {
    trickleDown(i);
  }
}

template<typename T>
void MinHeap<T>::trickleUp(const int index) {
  int parentIndex = getParentIndex(index);
  int thisChildIndex = index;

  // trickleUp until compared with root of heap
  while (m_heapArr[thisChildIndex] < m_heapArr[parentIndex] && parentIndex != -1) {
    // swap child and parent
    T temp = m_heapArr[parentIndex];
    m_heapArr[parentIndex] = m_heapArr[thisChildIndex];
    m_heapArr[thisChildIndex] = temp;

    // compare next parent
    thisChildIndex = parentIndex;
    parentIndex = getParentIndex(parentIndex);
  }
}

template<typename T>
void MinHeap<T>::trickleDown(const int index) {
  // initialize min value
  T minValue = m_heapArr[index];

  // compare with all children to see if any of them has lower value
  int minChildNum = 0;
  for (int i = 1; i <= m_k; i++) {
    int childIndex = getChildIndex(index, i);
    if (childIndex != -1 && childIndex < m_numEntries) {
      // get child
      T child = m_heapArr[childIndex];

      // compare and keep track if min changes
      if (child < minValue) {
        minValue = child;
        minChildNum = i;
      }
    }
  }

  // swap if there is a child with lower value
  if (minChildNum != 0) {
    int minChildIndex = getChildIndex(index, minChildNum);

    T temp = m_heapArr[minChildIndex];
    m_heapArr[minChildIndex] = m_heapArr[index];
    m_heapArr[index] = temp;

    // trickleDown again until height of heap
    trickleDown(minChildIndex);
  }
}

template<typename T>
void MinHeap<T>::insertItem(const T item) throw (PVE) {
  if (m_numEntries + 1 >= m_maxHeapSize)
    throw PVE("\nHeap full!\n");

  // insert the new item at leftmost opening
  m_heapArr[m_numEntries] = item;

  trickleUp(m_numEntries);

  m_numEntries++;
}

template<typename T>
void MinHeap<T>::removeItem(const T item) {
  while (removeDuplicates(item));
}

template<typename T>
bool MinHeap<T>::removeDuplicates(const T item) {
  for (int i = 0; i < m_numEntries; i++) {
    if (m_heapArr[i] == item) {
      // value is the last leaf
      if (i == m_numEntries) {
        m_heapArr[m_numEntries] = -1;
        m_numEntries--;
        return true;
      }

      // value isn't a leaf
      m_heapArr[i] = m_heapArr[m_numEntries - 1];
      m_heapArr[m_numEntries - 1] = -1;
      m_numEntries--;

      // trickleDown as need
      if (m_heapArr[i] < m_heapArr[getParentIndex(i)]) {
        trickleUp(i);
      } else if (m_heapArr[i] > m_heapArr[getParentIndex(i)]) {
        trickleDown(i);
      }

      return true;
    }
  }

  return false;
}

template<typename T>
void MinHeap<T>::deleteMin() throw (PVE) {
  if (m_numEntries == 0)
    throw PVE("\nHeap empty.\n");

  m_heapArr[0] = m_heapArr[m_numEntries - 1];
  m_heapArr[m_numEntries - 1] = -1;
  m_numEntries--;

  trickleDown(0);
}

template<typename T>
void MinHeap<T>::deleteMax() throw (PVE) {
  if (m_numEntries == 0)
    throw PVE("\nHeap empty.\n");

  int firstLeafIndex = floor((m_numEntries - 2) / m_k) + 1;
  int maxIndex = firstLeafIndex;
  for (int i = firstLeafIndex; i < m_numEntries; i++) {
    if (m_heapArr[i] > m_heapArr[maxIndex]) {
      maxIndex = i;
    }
  }

  m_heapArr[maxIndex] = m_heapArr[m_numEntries - 1];
  m_heapArr[m_numEntries - 1]= -1;
  m_numEntries--;
}

template<typename T>
void MinHeap<T>::levelorder() const {
  // first child level starts at index 1
  int nextLevel = 1;
  int childNum = 1;

  for (int i = 0; i < m_numEntries; i++) {
    bool levelFull = ((childNum != 3) || (i + 1 == nextLevel));
    bool isRightMostSubTree = m_heapArr[i + 1] == -1;

    // print element
    std::cout << m_heapArr[i];

    // print separator
    if (!levelFull && !isRightMostSubTree) {
      std::cout << " - ";
      childNum = 1;
    } else {
      std::cout << " ";
      childNum++;
    }

    // print children on new line
    if (i + 1 == nextLevel) {
      std::cout << "\n";
      nextLevel = getChildIndex(i + 1, 1);
      childNum = 1;
    }
  }

  std::cout << "\n";
}
