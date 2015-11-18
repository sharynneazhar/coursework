/**
*	@file : DoubleLinkedList.hpp
*	@author : Sharynne Azhar
*	@date : 2015.09.21
*	@brief: Implementation file for DoubleLinkedList class.
*/

#include <iostream>

template <typename T>
DoubleLinkedList<T>::DoubleLinkedList()
{
	m_front = nullptr;
	m_back = nullptr;
 	m_size = 0;
}

template <typename T>
DoubleLinkedList<T>::~DoubleLinkedList()
{
	Node<T>* nodetoDelete = m_front;
	Node<T>* nextNode = m_front;

	while (nextNode != nullptr)
	{
		nodetoDelete = nextNode;
		nextNode = nextNode->getNext();
		delete nodetoDelete;
	}

	nodetoDelete = nullptr;
	nextNode = nullptr;
}

template <typename T>
bool DoubleLinkedList<T>::isEmpty() const
{
	if (m_size == 0)
	{
		return (true);
	}
	else
	{
		return (false);
	}
}

template <typename T>
int DoubleLinkedList<T>::size() const
{
	return (m_size);
}

template <typename T>
void DoubleLinkedList<T>::pushFront(T value)
{
	if (isEmpty())
	{
		m_front = new Node<T>();
		m_front->setValue(value);
		m_size++;
	}
	else
	{
		Node<T>* temp = new Node<T>(); // creates new node
		temp->setValue(value); // sets value of new node

		temp->setNext(m_front); // sets next node to be m_front
		m_front->setPrev(temp);

		m_front = temp; // sets m_front to point to new front node

		m_size++;
	}
}

template <typename T>
void DoubleLinkedList<T>::pushBack(T value)
{
	if (isEmpty())
	{
		m_front = new Node<T>();
		m_front->setValue(value);
		m_size++;
	}
	else
	{
		Node<T>* traverse = m_front;
		Node<T>* temp = new Node<T>();
		temp->setValue(value);

		while (traverse->getNext() != nullptr) // traverses
		{
			traverse = traverse->getNext(); // get next node
		}

		// traverse is nullptr so link to temp
		traverse->setNext(temp);
		temp->setPrev(traverse);

		m_size++;
	}
}

template <typename T>
bool DoubleLinkedList<T>::removeBack()
{
	if (isEmpty())
	{
		return (false);
	}
	else
	{
		Node<T>* traverse = m_front;
		Node<T>* nextToLast = nullptr;

		while (traverse->getNext() != nullptr)
		{
			nextToLast = traverse;
			traverse = traverse->getNext();
		}

		delete traverse;
		traverse = nullptr;

		if (nextToLast != nullptr)
		{
			nextToLast->setNext(nullptr);
		}

		m_size--;

		return (true);
	}
}

template <typename T>
bool DoubleLinkedList<T>::removeFront()
{
	if (isEmpty())
	{
		return (false);
	}
	else
	{
		Node<T>* traverse = m_front;

		m_front = m_front->getNext();

		delete traverse;
		traverse = nullptr;

		m_size--;
		return (true);
	}
}

template <typename T>
bool DoubleLinkedList<T>::remove(T value)
{
	if (isEmpty())
	{
		return (false);
	}
	else if (m_size > 0)
	{
		Node<T>* toBeRemoved = find(value);

		if (toBeRemoved == nullptr)
		{
			return false;
		}
		else
		{
			Node<T>* prev = toBeRemoved->getPrev();
			Node<T>* next = toBeRemoved->getNext();

			if (m_front == toBeRemoved)
			{
				m_front = next;
			}

			if (prev != nullptr)
			{
				prev->setNext(next);
			}

			if (next != nullptr)
			{
				next->setPrev(prev);
			}

			delete toBeRemoved;
			toBeRemoved = nullptr;
			m_size--;

			return (true);
		}
	}
}

template <typename T>
void DoubleLinkedList<T>::insertAhead(T listValue, T newValue) throw (std::runtime_error)
{
	Node<T>* nextNode = find(listValue);

	if (isEmpty())
	{
		throw (std::runtime_error("List Empty"));
	}
	else if (nextNode == nullptr)
	{
		throw (std::runtime_error("Value not found"));
	}
	else
	{
		Node<T>* insertValue = new Node<T>();
		Node<T>* prevNode = nextNode->getPrev();

		insertValue->setValue(newValue);
		insertValue->setPrev(prevNode);
		insertValue->setNext(nextNode);

		if (m_front == nextNode)
		{
			m_front = insertValue;
		}

		if (prevNode != nullptr)
		{
			prevNode->setNext(insertValue);
		}

		nextNode->setPrev(insertValue);

		m_size++;
	}

}

template <typename T>
void DoubleLinkedList<T>::insertBehind(T listValue, T newValue) throw (std::runtime_error)
{
	Node<T>* prevNode = find(listValue);

	if (isEmpty())
	{
		throw (std::runtime_error("List Empty"));
	}
	else if (prevNode == nullptr)
	{
		throw (std::runtime_error("Value not found"));
	}
	else
	{
		Node<T>* insertValue = new Node<T>();
		Node<T>* nextNode = prevNode->getNext();

		insertValue->setValue(newValue);
		insertValue->setPrev(prevNode);
		insertValue->setNext(nextNode);

		prevNode->setNext(insertValue);

		if (nextNode != nullptr)
		{
			nextNode->setPrev(insertValue);
		}

		m_size++;
	}
}

template <typename T>
Node<T>* DoubleLinkedList<T>::find(T value) const
{
	Node<T>* findValue = m_front;

	while (findValue != nullptr)
	{
		if (findValue->getValue() == value)
		{
			return findValue;
		}
		findValue = findValue->getNext();
	}

	return (nullptr);
}

template <typename T>
void DoubleLinkedList<T>::printList() const
{
	Node<T>* traverse = m_front;

	while (traverse != nullptr)
	{
		std::cout << traverse->getValue() << " ";
		traverse = traverse->getNext();
	}

}
