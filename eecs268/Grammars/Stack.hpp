/**
*	@file : Stack.hpp
*	@author : Sharynne Azhar
*	@date : 02-17-2016
*	@brief: Implementation of the stack class
*/

template <typename T>
Stack<T>::Stack() : top(-1) {}

// Copy constructor and destructor are supplied by the compiler

template <typename T>
bool Stack<T>::isEmpty() const
{
    return top < 0;
}

template <typename T>
void Stack<T>::push(const T& newEntry) throw (PVE)
{
    if (top == MAX_STACK - 1)
    {
        throw PVE("Stack full!\n\n");
    }

    top++;
    items[top] = newEntry;
}

template <typename T>
void Stack<T>::pop() throw(PVE)
{
    if (isEmpty())
    {
        throw PVE("Stack is empty. Pop failed.\n\n");
    }

    top--;
}

template <typename T>
T Stack<T>::peek() const throw(PVE)
{
    if (isEmpty())
    {
        throw PVE("Stack is empty. Peek failed.\n\n");
    }

    return items[top];
}
