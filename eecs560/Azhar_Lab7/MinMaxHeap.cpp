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
}

template<typename T>
void MinMaxHeap<T>::trickleDownMin(int index) {

}

template<typename T>
void MinMaxHeap<T>::trickleDownMax(int index) {

}

template<typename T>
void MinMaxHeap<T>::bubbleUp(int index) {

}

template<typename T>
void MinMaxHeap<T>::bubbleUpMin(int index) {

}

template<typename T>
void MinMaxHeap<T>::bubbleUpMax(int index) {

}

template<typename T>
void MinMaxHeap<T>::insertItem(const T item) {
  // bubble up
}

template<typename T>
void MinMaxHeap<T>::deleteMin() {
  // trickle down min
}

template<typename T>
void MinMaxHeap<T>::deleteMax() {
  // trickle down max
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
