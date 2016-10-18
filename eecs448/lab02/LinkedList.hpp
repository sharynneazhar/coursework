/**
*	@author 
*	@date 
*	@file LinkedList.hpp
*	@brief Implementation file for templated LinkedList class
*/

template <typename T>
LinkedList<T>::LinkedList() : m_front(nullptr), m_size(0)
{

}

template <typename T>
LinkedList<T>::~LinkedList() 
{
	while(!isEmpty())
	{
		removeFront();
	}
}

template <typename T>
bool LinkedList<T>::isEmpty() const
{
	return(m_size == 0);
}

template <typename T>
int LinkedList<T>::size() const
{
	return (m_size);
}

template <typename T>
bool LinkedList<T>::search(T value) const
{
	Node<T>* traverse = m_front;

   	if (!isEmpty()) {
        	while (traverse->getNext() != nullptr) {
			if (traverse->getValue() == value) {
				return true;
            		}
		        traverse = traverse->getNext();
        	}

	        if (traverse->getValue() == value) {
		        return true;
	        }	
	}

	return false;
}

template <typename T>
std::vector<T> LinkedList<T>::toVector() const
{
	std::vector<T> vec;
	Node<T>* temp = m_front;

	while( temp != nullptr )
	{
		vec.push_back(temp->getValue());
		temp = temp->getNext();
	}

	return(vec);	
	
}

template <typename T>
void LinkedList<T>::addBack(T value)
{
	Node<T>* temp = nullptr;

	if(isEmpty())
	{
		m_front = new Node<T>(value);	
	}
	else
	{
		temp = m_front;
		while(temp->getNext() != nullptr)
		{
			temp = temp->getNext();
		}

		temp->setNext( new Node<T>(value) );		
	}

	m_size++;
}

template <typename T>
void LinkedList<T>::addFront(T value)
{
	Node<T>* temp = m_front;
	m_front = new Node<T>(value);
	m_front->setNext( temp );
	m_size++;
}

template <typename T>
bool LinkedList<T>::removeBack()
{
	Node<T>* traverse = m_front;
	Node<T>* secondintoLast = nullptr;
	bool isRemoved = false;

	if (!isEmpty()) {
		while (traverse->getNext() != nullptr) {
			secondintoLast = traverse;
			traverse = traverse->getNext();
		}

		delete traverse;
		traverse = nullptr;
		
		if (secondintoLast != nullptr)
			secondintoLast->setNext(nullptr);
		
		m_size--;
		isRemoved = true;
	}
	
	return (isRemoved);
}	

template <typename T>
bool LinkedList<T>::removeFront()
{
	Node<T>* temp = nullptr;
	bool isRemoved = false;

	if(!isEmpty())
	{
		temp = m_front;
		m_front = temp->getNext();
		delete temp;
		m_size--;
		isRemoved = true;
	}

	return(isRemoved);
}
