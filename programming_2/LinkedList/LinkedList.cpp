/**
*	@file : LinkedList.cpp
*	@author : Sharynne Azhar
*	@date : 02-08-2016
*	Purpose: Implementation file for the LinkedList class
*/

#include "LinkedList.h"

LinkedList::LinkedList()
{
    m_front = nullptr;
    m_size = 0;
}

LinkedList::~LinkedList()
{
    // removes all nodes until list is empty
    while(removeFront());
    m_front = nullptr;
}

int LinkedList::size() const
{
    return m_size;
}

bool LinkedList::isEmpty() const
{
    if (m_size == 0)
    {
        return true;
    }

    return false;
}

bool LinkedList::search(int value) const
{
    Node* traverse = m_front;

    if (!isEmpty())
    {
        while (traverse->getNext() != nullptr)
        {
            if (traverse->getValue() == value)
            {
                return true;
            }
            traverse = traverse->getNext();
        }

        if (traverse->getValue() == value)
        {
            return true;
        }
    }

    return false;
}

void LinkedList::printList() const
{
    Node* traverse = m_front;

    if (isEmpty())
    {
        std::cout << "List is empty\n";
    }
    else
    {
        while (traverse != nullptr)
        {
            std::cout << traverse->getValue() << " ";
            traverse = traverse->getNext();
        }
    }
}

void LinkedList::addBack(int value)
{
    Node* traverse = m_front;

    Node* newNode = new Node();
    newNode->setValue(value);

    if (isEmpty())
    {
        m_front = newNode;
    }
    else
    {
        while (traverse->getNext() != nullptr)
        {
            traverse = traverse->getNext();
        }

        traverse->setNext(newNode);
    }

    m_size++;

    newNode = nullptr;
}

void LinkedList::addFront(int value)
{
    Node* newNode = new Node();
    newNode->setValue(value);

    if (m_front == nullptr)
    {
        m_front = newNode;
    }
    else
    {
        newNode->setNext(m_front);
        m_front = newNode;
    }

    m_size++;

    newNode = nullptr;
}

bool LinkedList::removeBack()
{
    Node* traverse = m_front;
    Node* nextToLastNode = nullptr;

	if (!isEmpty())
	{
        while(traverse->getNext() != nullptr)
        {
            nextToLastNode = traverse;
            traverse = traverse->getNext();
        }

        delete traverse;
        traverse = nullptr;

        if (nextToLastNode != nullptr)
        {
            nextToLastNode->setNext(nullptr);
        }

        m_size--;
        return true;
	}

	return false;
}

bool LinkedList::removeFront()
{
    Node* traverse = m_front;

	if (!isEmpty())
	{
        m_front = m_front->getNext();

        delete traverse;
        traverse = nullptr;

        m_size--;
		return true;
	}

	return false;
}

std::vector<int> LinkedList::toVector() const
{
    Node* traverse = m_front;
    std::vector<int> vec;

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

void LinkedList::removeDuplicates() {
    Node* traverse = m_front;
    Node* duplicate = nullptr;

    if (!isEmpty())
    {
        Node* nextNode = traverse->getNext();
        while(nextNode != nullptr)
        {
            std::cout << "\nCurrent Node: " << traverse->getValue() << "\n";
            std::cout << "Next Node: " << nextNode->getValue() << "\n";

            if (traverse->getValue() == nextNode->getValue())
            {
                std::cout << "Found duplicate\n";
                duplicate = nextNode;
                nextNode = nextNode->getNext();

                std::cout << "Deleting duplicate: " << duplicate->getValue() << "\n";
                delete duplicate;
                duplicate = nullptr;

                traverse->setNext(nextNode);
            }

            traverse = nextNode;
            nextNode = nextNode->getNext();
        }
    }
}

int LinkedList::count(int value)
{
    Node* traverse = m_front;
    int count = 0;

    while (traverse != nullptr)
    {
        if (value == traverse->getValue())
        {
            count++;
        }

        traverse = traverse->getNext();
    }

    return count;
}

void LinkedList::insertAhead(int value, int listValue)
{
    Node* traverse = m_front->getNext();
    Node* prevNode = m_front;

    Node* newNode = new Node();
    newNode->setValue(value);

    if (listValue == m_front->getValue())
    {
        newNode->setNext(m_front);
        m_front = newNode;
    }
    else
    {
        while (traverse != nullptr)
        {
            if (traverse->getValue() == listValue)
            {
                newNode->setNext(traverse);
                prevNode->setNext(newNode);
            }

            prevNode = traverse;
            traverse = traverse->getNext();
        }
    }

    newNode = nullptr;
}
