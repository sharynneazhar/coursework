/**
*	@file : Node.hpp
*	@author : Sharynne Azhar
*	@date : 02-08-2016
*	Purpose: implementation of the Node class
*/

template <typename T>
Node<T>::Node()
{
    m_value = T();
    m_next = nullptr;
}

template <typename T>
T Node<T>::getValue() const
{
    return (m_value);
}

template <typename T>
void Node<T>::setValue(T val)
{
    m_value = val;
}

template <typename T>
Node<T>* Node<T>::getNext() const
{
    return (m_next);
}

template <typename T>
void Node<T>::setNext(Node<T>* next)
{
    m_next = next;
}
