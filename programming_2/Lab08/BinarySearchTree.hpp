/**
*	@file : BinarySearchTree.hpp
*	@author : Sharynne Azhar
*	@date : 2015.11.08
*	@brief: Implementation file for BST class
*/

template <typename T>
BinarySearchTree<T>::BinarySearchTree()
{
    m_root = nullptr;
}

template <typename T>
BinarySearchTree<T>::BinarySearchTree(const BinarySearchTree<T>& other)
{
    if (other.isEmpty())
    {
        m_root = nullptr;
    }
    else
    {
        m_root = new Node<T>(*(other.m_root));
    }
}

template<typename T>
BinarySearchTree<T>::~BinarySearchTree()
{
	deleteTree(m_root);
}

template<typename T>
BSTI<T>* BinarySearchTree<T>::clone()
{
	BSTI<T>* clone = new BinarySearchTree<T>(*(this));
	return clone;
}

template<typename T>
bool BinarySearchTree<T>::isEmpty() const
{
	return m_root == nullptr;
}

template<typename T>
bool BinarySearchTree<T>::add(T value)
{
	return add(value, m_root);
}

template<typename T>
bool BinarySearchTree<T>::search(T value) const
{
	if(isEmpty())
    {
		return false;
	}
	else
    {
		return search(value, m_root);
	}
}

template<typename T>
void BinarySearchTree<T>::printTree(Order order) const
{
	printTree(m_root, order);
}

template<typename T>
std::vector<T> BinarySearchTree<T>::treeToVector(Order order) const
{
	std::vector<T> vec;
	treeToVector(m_root, order, vec);
    return vec;
}

template<typename T>
bool BinarySearchTree<T>::add(T value, Node<T>* subTree)
{
	//if tree is empty
	if(isEmpty())
    {
		//create new node and place value
		Node<T>* newNode = new Node<T>();
		newNode->setValue(value);
		m_root = newNode;

		return true;
	}

	//if the value is less than the value in the subtree
	else if(value < subTree->getValue())
    {
        if (subTree->getLeft() == nullptr)
        {
			Node<T>* newNode = new Node<T>();
			newNode->setValue(value);
			subTree->setLeft(newNode);

			return true;
		}

		return add(value, subTree->getLeft());
	}
	else if (value > subTree->getValue())
    {
		if(subTree->getRight() == nullptr)
        {
			Node<T>* newNode = new Node<T>();
			newNode->setValue(value);
			subTree->setRight(newNode);

			return true;
		}

		return add(value,subTree->getRight());
	}
	else
    {
		return false;
	}
}

template<typename T>
void BinarySearchTree<T>::deleteTree(Node<T>* subTree)
{
	if (subTree != nullptr)
    {
		if (subTree->getLeft() != nullptr)
        {
			deleteTree(subTree->getLeft());
		}
		if (subTree->getRight() != nullptr)
        {
			deleteTree(subTree->getRight());
		}
	}

	delete subTree;
	subTree = nullptr;
}

template<typename T>
bool BinarySearchTree<T>::search(T value, Node<T>* subTree) const{
	//Tree has been checked as not empty.
	if (value < subTree -> getValue())
    {
		if (subTree -> getLeft() != nullptr)
        {
			return search(value,subTree->getLeft());
		}
		else
        {
			return false;
		}
	}
	else if (value > subTree -> getValue())
    {
		if (subTree -> getRight() != nullptr)
        {
			return search(value,subTree->getRight());
		}
		else
        {
			return false;
		}
	}
	else if (value == subTree->getValue())
    {
		return true;
	}
	else
    {
		return false;
	}
}

template<typename T>
void BinarySearchTree<T>::printTree(Node<T>* subTree, Order order) const
{
	switch(order)
    {
    	case PRE_ORDER:
        {
    		if (subTree != nullptr)
            {
    			std::cout << subTree->getValue() << " ";
    			printTree(subTree->getLeft(), order);
    			printTree(subTree->getRight(), order);
    		}
    		break;
        }
    	case IN_ORDER:
    	{
    		if (subTree != nullptr)
            {
    			printTree(subTree->getLeft(), order);
    			std::cout << subTree->getValue() << " ";
    			printTree(subTree->getRight(), order);
    		}
    		break;
    	}
    	case POST_ORDER:
        {
        	if (subTree != nullptr)
            {
        		printTree(subTree->getLeft(), order);
        		printTree(subTree->getRight(), order);
        		std::cout << subTree->getValue() << " ";
        	}
        	break;
        }
	} // end switch
}

template<typename T>
void BinarySearchTree<T>::treeToVector(Node<T>* subTree, Order order, std::vector<T>& vec) const
{
	switch(order)
    {
		case PRE_ORDER:
		{
            if (subTree != nullptr)
            {
    			vec.push_back(subTree->getValue());
    			treeToVector(subTree->getLeft(), order, vec);
    			treeToVector(subTree->getRight(), order, vec);
    		}
		     break;
         }
		case IN_ORDER:
		{
    		if (subTree != nullptr)
            {
    		    treeToVector(subTree->getLeft(), order, vec);
    			vec.push_back(subTree->getValue());
    			treeToVector(subTree->getRight(), order, vec);
    		}
    		break;
		}
		case POST_ORDER:
		{
            if (subTree != nullptr)
            {
    			treeToVector(subTree->getLeft(), order, vec);
    			treeToVector(subTree->getRight(), order, vec);
    			vec.push_back(subTree->getValue());
    		}
    		break;
        }
	} // end switch
}
