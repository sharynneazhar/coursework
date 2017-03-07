/**
*	@file   : MinMaxHeap.cpp
*	@author : Sharynne Azhar
*	@date   : 03-07-2017
* @desc   : Implementation file for the min-max-heap class
*/

#define MAX_HEAP_SIZE 1000;

template<typename T>
MinMaxHeap<T>::MinMaxHeap() {
  m_heapArr = new T[0];
}

template<typename T>
MinMaxHeap<T>::MinMaxHeap(const T values[], const int numEntries) {
  m_maxHeapSize = MAX_HEAP_SIZE + 1;
  m_numEntries = numEntries;

  m_heapArr = new T[m_maxHeapSize];

  for (int i = 0; i < m_numEntries; i++) {
    m_heapArr[i] = values[i];
  }

  for (int i = m_numEntries + 1; i < m_maxHeapSize; i++) {
    m_heapArr[i] = -1;
  }

  buildHeap();
}

template<typename T>
MinMaxHeap<T>::~MinMaxHeap() {
  delete [] m_heapArr;
}

template<typename T>
void MinMaxHeap<T>::buildHeap() {
  // trickle min
  // trickle max

}

template<typename T>
void MinMaxHeap<T>::trickleDown(int index) {

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
void MinMaxHeap<T>::removeItem(const T item) {

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
void MinMaxHeap<T>::levelorder() const {
  Queue<T> queue;


  std::cout << std::endl;
}
