/**
*	@file : Queue.hpp
*	@author : Sharynne Azhar
*	@date : 02-22-2016
*	@brief: Implementation of the queue class
*/

template <typename T>
Queue<T>::Queue()
{
    m_size = 0;
    m_front = nullptr;
    m_back = nullptr;
}

template <typename T>
Queue<T>::~Queue()
{
    while (!isEmpty())
    {
        dequeue();
    }
}

template <typename T>
bool Queue<T>::isEmpty() const
{
    if (m_size == 0)
    {
        return true;
    }

    return false;
}

template <typename T>
void Queue<T>::enqueue(const T& newEntry) throw (PrecondViolatedExcep)
{
    if (isEmpty())
    {
        m_front = new Node<T>();
        m_front->setValue(newEntry);
        m_back = m_front;
        m_size++;
    }
    else
    {
        Node<T>* newNode = new Node<T>();
        newNode->setValue(newEntry);

        m_back->setNext(newNode);
        m_back = newNode;
        m_size++;

        newNode = nullptr;
    }

}

template <typename T>
void Queue<T>::dequeue() throw (PrecondViolatedExcep)
{
    Node<T>* nodeToDequeue = nullptr;

    if (isEmpty())
    {
        throw PrecondViolatedExcep("No one waiting in queue\n");
    }
    else
    {
        nodeToDequeue = m_front;
        m_front = m_front->getNext();
        m_size--;

        delete nodeToDequeue;
        nodeToDequeue = nullptr;
    }
}

template <typename T>
T Queue<T>::peekFront() const throw (PrecondViolatedExcep)
{
    if (isEmpty())
    {
        throw PrecondViolatedExcep("No one left in queue\n");
    }
    else
    {
        return m_front->getValue();
    }
}

template <typename T>
T Queue<T>::getNext() const throw (PrecondViolatedExcep)
{
    if (isEmpty())
    {
        throw PrecondViolatedExcep("No one left in queue\n");
    }
    else
    {
        Node<T>* nextNode = m_front->getNext();
        return nextNode->getValue();
    }
}

template <typename T>
void Queue<T>::print() const
{
    Node<T>* nodeToPrint = m_front;

    while (nodeToPrint != nullptr)
    {
        std::cout << nodeToPrint->getValue() << " ";
        nodeToPrint = nodeToPrint->getNext();
    }

    std::cout << "\n";
}
