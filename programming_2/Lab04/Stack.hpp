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
    if (m_size != 0)
    {
        return false;
    }

    return true;
}

template <typename T>
void Stack<T>::push(const T newEntry)
{
    if (isEmpty())
    {
        m_top = new Node<T>();
        m_top->setValue(newEntry);
        m_size++;
    }
    else
    {
        Node<T>* nodeToAdd = new Node<T>(); // creates a new node
        nodeToAdd->setValue(newEntry); // set the value

        // reassign next and prev nodes to preserve order
        nodeToAdd->setNext(m_top);
        m_top->setPrev(nodeToAdd);

        // reassign m_top to the first node in the stack
        m_top = nodeToAdd;
        m_size++;
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
        // set the new node to the top node
        Node<T>* topNodeToDelete = m_top;

        // re-assign the top node to the next node in stack
        m_top = m_top->getNext();

        std::cout << topNodeToDelete->getValue() << " removed from stack 1";

        // delete node
        delete topNodeToDelete;
        topNodeToDelete = nullptr;

        m_size--;
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
void Stack<T>::print() const
{
    Node<T>* nodeToPrint = m_top;

    while (nodeToPrint != nullptr)
    {
        std::cout << nodeToPrint->getValue() << " ";
        nodeToPrint = nodeToPrint->getNext();
    }
}
