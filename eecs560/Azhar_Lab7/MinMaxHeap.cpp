/**
*	@file   : MinMaxHeap.cpp
*	@author : Sharynne Azhar
*	@date   : 03-07-2017
* @desc   : Implementation file for the min-max-heap class
*           Reference - https://people.eecs.ku.edu/~mhajiarb/minmax1.pdf
*/

template<typename T>
MinMaxHeap<T>::MinMaxHeap() {
  m_heapArr = new T[0];
}

template<typename T>
MinMaxHeap<T>::MinMaxHeap(const T values[], const int numEntries) {
  m_maxHeapSize = MAX_HEAP_SIZE + 1;
  m_numEntries = numEntries;
  m_heapArr = new T[m_maxHeapSize];
  for (int i = 0; i < m_maxHeapSize; i++) {
    m_heapArr[i] = -1;
  }
  buildHeap(values);
}

template<typename T>
MinMaxHeap<T>::~MinMaxHeap() {
  delete [] m_heapArr;
}

template<typename T>
int MinMaxHeap<T>::getParentIndex(int index) {
  return floor(index / 2);
}

template<typename T>
int MinMaxHeap<T>::getLeftChildIndex(int index) {
  return 2 * index;
}

template<typename T>
int MinMaxHeap<T>::getRightChildIndex(int index) {
  return 2 * index + 1;
}

template<typename T>
void MinMaxHeap<T>::swap(int index1, int index2) {
  T temp = m_heapArr[index1];
  m_heapArr[index1] = m_heapArr[index2];
  m_heapArr[index2] = temp;
}

template<typename T>
void MinMaxHeap<T>::buildHeap(const T values[]) {
  for (int i = 0; i < m_numEntries; i++) {
      m_heapArr[i + 1] = values[i];
  }

  for (int i = m_numEntries; i != 0; i--) {
    trickleDown(i);
  }
}

template<typename T>
void MinMaxHeap<T>::trickleDown(int index) {
  if ((int) floor(log2(index)) % 2 == 0) {
    // min node if floor(lg(i)) is even
    trickleDownMin(index);
  } else {
    // max node if floor(lg(i)) is odd
    trickleDownMax(index);
  }
}

template<typename T>
void MinMaxHeap<T>::trickleDownMin(int index) {
  // get left child and its children
  int lc = getLeftChildIndex(index);
  int lclc = getLeftChildIndex(lc);
  int lcrc = getRightChildIndex(lc);

  // get right child and its children
  int rc = getRightChildIndex(index);
  int rclc = getLeftChildIndex(rc);
  int rcrc = getRightChildIndex(rc);

  // the index of the smallest of the children and grandchildren
  int m = 0;

  // find the smallest of children and grandchildren
  if (lc < m_numEntries + 1 && lc != -1)
    m = lc;
  if (rc < m_numEntries + 1 && rc != -1 && m_heapArr[rc] < m_heapArr[m])
    m = rc;
  if (lclc < m_numEntries + 1 && lclc != -1 && m_heapArr[lclc] < m_heapArr[m])
    m = lclc;
  if (lcrc < m_numEntries + 1 && lcrc != -1 && m_heapArr[lcrc] < m_heapArr[m])
    m = lcrc;
  if (rclc < m_numEntries + 1 && rclc != -1 && m_heapArr[rclc] < m_heapArr[m])
    m = rclc;
  if (rcrc < m_numEntries + 1 && rcrc != -1 && m_heapArr[rcrc] < m_heapArr[m])
    m = rcrc;

  // do swaps as needed
  if (m != 0) {
    if (m == lc || m == rc) {
      // m is a child
      if (m_heapArr[m] < m_heapArr[index])
        swap(m, index);
    } else {
      // m is a grandchild
      if (m_heapArr[m] < m_heapArr[index]) {
        swap(m, index);
        int parent = getParentIndex(m);
        if (m_heapArr[m] > m_heapArr[parent])
          swap(m, parent);
      }
      trickleDownMin(m);
    }
  }
}

template<typename T>
void MinMaxHeap<T>::trickleDownMax(int index) {
  // get left child and its children
  int lc = getLeftChildIndex(index);
  int lclc = getLeftChildIndex(lc);
  int lcrc = getRightChildIndex(lc);

  // get right child and its children
  int rc = getRightChildIndex(index);
  int rclc = getLeftChildIndex(rc);
  int rcrc = getRightChildIndex(rc);

  // the index of the largest of the children and grandchildren
  int m = 0;

  // find the smallest of children and grandchildren
  if (lc < m_numEntries + 1 && lc != -1)
    m = lc;
  if (rc < m_numEntries + 1 && rc != -1 && m_heapArr[rc] > m_heapArr[m])
    m = rc;
  if (lclc < m_numEntries + 1 && lclc != -1 && m_heapArr[lclc] > m_heapArr[m])
    m = lclc;
  if (lcrc < m_numEntries + 1 && lcrc != -1 && m_heapArr[lcrc] > m_heapArr[m])
    m = lcrc;
  if (rclc < m_numEntries + 1 && rclc != -1 && m_heapArr[rclc] > m_heapArr[m])
    m = rclc;
  if (rcrc < m_numEntries + 1 && rcrc != -1 && m_heapArr[rcrc] > m_heapArr[m])
    m = rcrc;

  // do swaps as needed
  if (m != 0) {
    if (m == lc || m == rc) {
      // m is a child
      if (m_heapArr[m] > m_heapArr[index])
        swap(m, index);
    } else {
      // m is a grandchild
      if (m_heapArr[m] > m_heapArr[index]) {
        swap(m, index);
        int parent = getParentIndex(m);
        if (m_heapArr[m] < m_heapArr[parent])
          swap(m, parent);
      }
      trickleDownMax(m);
    }
  }
}

template<typename T>
void MinMaxHeap<T>::bubbleUp(int index) {
  int parent = getParentIndex(index);
  if ((int) floor(log2(index)) % 2 == 0) {
    if (parent != -1 && m_heapArr[index] > m_heapArr[parent]) {
      swap(index, parent);
      bubbleUpMax(parent);
    } else {
      bubbleUpMin(index);
    }
  } else {
    if (parent != -1 && m_heapArr[index] < m_heapArr[parent]) {
      swap(index, parent);
      bubbleUpMin(parent);
    } else {
      bubbleUpMax(index);
    }
  }
}

template<typename T>
void MinMaxHeap<T>::bubbleUpMin(int index) {
  int parent = getParentIndex(index);
  int grandparent = getParentIndex(parent);
  if (grandparent != -1 && m_heapArr[index] < m_heapArr[grandparent]) {
    swap(index, grandparent);
    bubbleUpMin(grandparent);
  }
}

template<typename T>
void MinMaxHeap<T>::bubbleUpMax(int index) {
  int parent = getParentIndex(index);
  int grandparent = getParentIndex(parent);
  if (grandparent != -1 && m_heapArr[index] > m_heapArr[grandparent]) {
    swap(index, grandparent);
    bubbleUpMin(grandparent);
  }
}

template<typename T>
void MinMaxHeap<T>::insertItem(const T item) {
  if (m_numEntries < m_maxHeapSize) {
    m_heapArr[m_numEntries + 1] = item;
    bubbleUp(m_numEntries + 1);
    m_numEntries++;
  } else {
    std::cout << "\nHeap is full.\n";
  }
}

template<typename T>
void MinMaxHeap<T>::deleteMin() {
  // trickle down min
  if (m_numEntries == 0) {
    std::cout << "\nHeap is empty.\n";
  } else if (m_numEntries == 1) {
    m_heapArr[1] = -1;
    m_numEntries--;
  } else {
    m_heapArr[1] = m_heapArr[m_numEntries];
    m_heapArr[m_numEntries] = -1;
    m_numEntries--;
    trickleDownMin(1);
  }
}

template<typename T>
void MinMaxHeap<T>::deleteMax() {
  // trickle down max
  if (m_numEntries == 0) {
    std::cout << "\nHeap is empty.\n";
  } else if (m_numEntries == 1) {
      m_heapArr[1] = -1;
      m_numEntries--;
  } else {
    if (m_heapArr[2] > m_heapArr[3]) {
      m_heapArr[2] = m_heapArr[m_numEntries];
      m_heapArr[m_numEntries] = -1;
      trickleDownMax(2);
    } else {
      m_heapArr[3] = m_heapArr[m_numEntries];
      m_heapArr[m_numEntries] = -1;
      trickleDownMax(3);
    }
    m_numEntries--;
  }
}

template<typename T>
void MinMaxHeap<T>::levelorder() {
  Queue<int> queue;

  // if the node type (min or max) changes then we know the level changes
  // let true be one type and false be the other
  bool lastNodeType = false;
  bool currNodeType = false;

  // set the root to be first index to visit
  int indexToVisit = 1;
  queue.enqueue(1);

  do {
    indexToVisit = queue.peek();

    // check if this is a min or max node
    currNodeType = ((int) floor(log2(indexToVisit)) % 2 == 0);

    // print end line if the node type changes
    if (currNodeType != lastNodeType) {
      std::cout << "\n";
    }

    std::cout << m_heapArr[indexToVisit] << " ";

    int lc = getLeftChildIndex(indexToVisit);
    int rc = getRightChildIndex(indexToVisit);

    if (lc < m_numEntries + 1 && lc != -1) {
      queue.enqueue(lc);
    }

    if (rc < m_numEntries + 1 && rc != -1) {
      queue.enqueue(rc);
    }

    lastNodeType = currNodeType;
    queue.dequeue();
  } while (!queue.isEmpty());

  std::cout << std::endl;
}
