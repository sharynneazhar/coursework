/**
*	@file   : Queue.cpp
*	@author : Sharynne Azhar
*	@date   : 03-27-2017
* @desc   : Implementation of Queue class
*/

template<typename T>
Queue<T>::Queue() : m_size(0), m_front(nullptr), m_back(nullptr) {}

template<typename T>
Queue<T>::~Queue() {
  while(!isEmpty()) {
    dequeue();
  }
}

template<typename T>
bool Queue<T>::isEmpty() const {
  return m_size == 0;
}

template<typename T>
void Queue<T>::enqueue(const T& item) {
  Node<T>* newNode = new Node<T>(item);

  if (isEmpty()) {
    m_front = newNode;
    m_back = m_front;
  } else {
    m_back->setNext(newNode);
    m_back = newNode;
  }

  m_size++;
}

template<typename T>
void Queue<T>::dequeue() throw (PVE) {
  Node<T>* nodeToDequeue = nullptr;
  if (isEmpty()) {
    throw PVE("\nDequeue failed. ERROR: Queue empty.\n");
  }

  nodeToDequeue = m_front;
  m_front = m_front->getNext();

  m_size--;

  delete nodeToDequeue;
  nodeToDequeue = nullptr;
}

template<typename T>
T Queue<T>::peek() const throw (PVE) {
  if (isEmpty()) {
    throw PVE("\nPeek failed. ERROR: Queue empty.\n");
  }

  return m_front->getValue();
}

template<typename T>
T Queue<T>::getNext() const throw (PVE) {
  if (isEmpty()) {
    throw PVE("\nGet next failed. ERROR: Queue empty.\n");
  }

  return m_front->getNext()->getValue();
}

template<typename T>
int Queue<T>::getSize() const {
  return m_size;
}

template<typename T>
void Queue<T>::print() const {
  Node<T>* traverser = m_front;
  std::cout << std::endl;
  while (traverser) {
    std::cout << traverser->getValue() << " ";
    traverser = traverser->getNext();
  }
  std::cout << std::endl;
}
