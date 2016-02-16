/**
*	@file : Stack.hpp
*	@author : Sharynne Azhar
*	@date : 2015.10.05
*	@brief: Implementation of the stack class
*/

template <typename T>
Stack<T>::Stack()
{
    m_size = 0;
    m_top = nullptr;
}

template <typename T>
Stack<T>::~Stack()
{
    // pop until stack is empty
    while (!isEmpty())
    {
        pop();
    }
}

template <typename T>
bool Stack<T>::isEmpty() const
{
    if (m_size == 0)
    {
        return true;
    }

    return false;
}

template <typename T>
void Stack<T>::push(const T value)
{
    if (isEmpty())
    {
        m_top = new Node<T>();
        m_top->setValue(value);
        m_size++;
    }
    else
    {
        Node<T>* nodeToAdd  = new Node<T>();
        nodeToAdd->setValue(value);
        nodeToAdd->setNext(m_top);
        m_top = nodeToAdd;

        m_size++;
        nodeToAdd = nullptr;
    }
}

template <typename T>
void Stack<T>::pop() throw(PreconditionViolationException)
{
    if (isEmpty())
    {
        throw PreconditionViolationException("Pop attempted on an empty stack");
    }
    else
    {
        Node<T>* nodeToDelete = m_top;
        m_top = m_top->getNext();
        m_size--;

        delete nodeToDelete;
        nodeToDelete = nullptr;
    }
}

template <typename T>
T Stack<T>::peek() const throw(PreconditionViolationException)
{
    if (isEmpty())
    {
        throw PreconditionViolationException("Peek attempted on an empty stack");
    }
    else
    {
        return m_top->getValue();
    }
}

template <typename T>
int Stack<T>::size() const
{
    return m_size;
}

template <typename T>
std::vector<T> Stack<T>::toVector() const
{
    std::vector<T> vec;
    Node<T>* traverse = m_top;

    if (!isEmpty())
    {
        while (traverse->getNext() != nullptr)
        {
            vec.push_back(traverse->getValue());
            traverse = traverse->getNext();
        }

        if (traverse != nullptr)
        {
                vec.push_back(traverse->getValue());
        }
    }

    return vec;
}
