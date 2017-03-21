/** @file libpriqueue.c
 */

#include <stdlib.h>
#include <stdio.h>

#include "libpriqueue.h"


/**
  Initializes the priqueue_t data structure.

  Assumtions
    - You may assume this function will only be called once per instance of
			priqueue_t
    - You may assume this function will be the first function called using an
			instance of priqueue_t.

  @param q a pointer to an instance of the priqueue_t data structure
  @param comparer a function pointer that compares two elements.
  See also @ref comparer-page
 */
void priqueue_init(priqueue_t *q, int(*comparer)(const void *, const void *)) {
				q->m_head = NULL;
				q->m_size = 0;
				q->m_comparer = comparer;
}


/**
  Inserts the specified element into this priority queue.

  @param q a pointer to an instance of the priqueue_t data structure
  @param ptr a pointer to the data to be inserted into the priority queue
  @return The zero-based index where ptr is stored in the priority queue, where
					0 indicates that ptr was stored at the front of the priority queue.
 */
int priqueue_offer(priqueue_t *q, void *ptr) {

				// create a new node object and set the element
				node_t *newNode = malloc(sizeof(node_t));
				newNode->m_ptr = ptr;
				newNode->m_next = NULL;

				// if the queue is empty, just add to front
				if (q->m_size == 0) {
								q->m_head = newNode;
								q->m_size++;
								return 0;
				}

				// else, traverse to find an open spot and prioritize
				int indexToReturn = 0;
				node_t *currNode = q->m_head;

				while (currNode != NULL) {
								void *newPtr = newNode->m_ptr;
								void *traverserPtr = currNode->m_ptr;
								int comparisonResult = q->m_comparer(newPtr, traverserPtr);

								// if the comparisonResult is less than 0,
								// then the new element has greater priority
								if (comparisonResult < 0) {
												// shift all elements down by swapping
												while (currNode != NULL) {
																node_t *tempNode = currNode->m_ptr;
																currNode->m_ptr = newNode->m_ptr;
																newNode->m_ptr = tempNode;

																// if end of queue, just add it in
																if (currNode->m_next == NULL) {
																				currNode->m_next = newNode;
																				break;
																}

																// keep traversing
																currNode = currNode->m_next;
												}

												break;
								}

								// comparison false, move to next index
								indexToReturn++;

								// if end of queue, just add it in
								if (currNode->m_next == NULL) {
												currNode->m_next = newNode;
												break;
								}

								// keep traversing
								currNode = currNode->m_next;
				}

				q->m_size++;
				return indexToReturn;
}


/**
  Retrieves, but does not remove, the head of this queue, returning NULL if
  this queue is empty.

  @param q a pointer to an instance of the priqueue_t data structure
  @return pointer to element at the head of the queue
  @return NULL if the queue is empty
 */
void *priqueue_peek(priqueue_t *q) {
				if (q->m_size > 0) {
								return q->m_head;
				}

				return NULL;
}


/**
  Retrieves and removes the head of this queue, or NULL if this queue
  is empty.

  @param q a pointer to an instance of the priqueue_t data structure
  @return the head of this queue
  @return NULL if this queue is empty
 */
void *priqueue_poll(priqueue_t *q) {
				if (q->m_size != 0) {
								node_t *tempNode = q->m_head;

								// remove the current head
								// point to next element or NULL if queue will be empty
								if (q->m_head->m_next != NULL) {
												q->m_head = q->m_head->m_next;
								} else {
												q->m_head = NULL;
								}

								// free the pointer
								void *freeNode = tempNode->m_ptr;
								free(tempNode);
								q->m_size--;
								return freeNode;
				}

				return NULL;
}


/**
  Returns the element at the specified position in this list, or NULL if
  the queue does not contain an index'th element.

  @param q a pointer to an instance of the priqueue_t data structure
  @param index position of retrieved element
  @return the index'th element in the queue
  @return NULL if the queue does not contain the index'th element
 */
void *priqueue_at(priqueue_t *q, int index) {
				// make sure the index is within the boundaries of the queue
				if (index >= 0 && index <= (int) q->m_size) {
								// start traversal from head of queue which has index 0
								node_t *currNode = q->m_head;
								int currIndex = 0;

								while (currNode != NULL) {
												// if found, return pointer
												if (currIndex == index) {
																return currNode->m_ptr;
												}

												// keep traversing
												currNode = currNode->m_next;
												currIndex++;
								}
				}

				return NULL;
}


/**
  Removes all instances of ptr from the queue.

  This function should not use the comparer function, but check if the data
	contained in each element of the queue is equal (==) to ptr.

  @param q a pointer to an instance of the priqueue_t data structure
  @param ptr address of element to be removed
  @return the number of entries removed
 */
int priqueue_remove(priqueue_t *q, void *ptr) {
				if (q->m_size != 0) {
								int count = 0;
								node_t *currNode = q->m_head;
								node_t *prevNode = q->m_head;

								// start traversing through queue
								while (currNode != NULL) {
												if (currNode->m_ptr == ptr) {
																// if head then set the new head to next
																// else just connect the previous to next node
																if (currNode == q->m_head) {
																				q->m_head = currNode->m_next;
																				prevNode = q->m_head;
																} else {
																				prevNode->m_next = currNode->m_next;
																}

																free(currNode);
																count++;
												}

												// keep traversing
												currNode = currNode->m_next;
								}

								q->m_size -= count;
								return count;
				}

				return 0;
}


/**
  Removes the specified index from the queue, moving later elements up
  a spot in the queue to fill the gap.

  @param q a pointer to an instance of the priqueue_t data structure
  @param index position of element to be removed
  @return the element removed from the queue
  @return NULL if the specified index does not exist
 */
void *priqueue_remove_at(priqueue_t *q, int index) {
				if (q->m_size != 0) {
								// check if element is at said index
								if (priqueue_at(q, index) != NULL) {
												// get the element
												node_t *nodeToDelete = priqueue_at(q, index);
												void *valuePtr = nodeToDelete->m_ptr;

												// if there is only one element
												if (nodeToDelete == q->m_head && q->m_size == 1) {
																q->m_head = NULL;
												} else if (nodeToDelete == q->m_head && q->m_size > 1) {
																// if head
																q->m_head = nodeToDelete->m_next;
												} else {
																// if somewhere in within the queue
																node_t *prevNode = priqueue_at(q, index - 1);
																node_t *nextNode = priqueue_at(q, index + 1);
																prevNode->m_next = nextNode;
												}

												free(nodeToDelete);
												q->m_size--;
												return valuePtr;
								}
				}

				return 0;
}


/**
  Returns the number of elements in the queue.

  @param q a pointer to an instance of the priqueue_t data structure
  @return the number of elements in the queue
 */
int priqueue_size(priqueue_t *q) {
				return q->m_size;
}


/**
  Destroys and frees all the memory associated with q.

  @param q a pointer to an instance of the priqueue_t data structure
 */
void priqueue_destroy(priqueue_t *q)
{
				// make sure queue isn't empty
				if (q->m_size > 0) {
								node_t *currNode = q->m_head;
								node_t *nextNode = q->m_head;

								// traverse the queue and delete as needed
								while (currNode != NULL) {
												nextNode = currNode->m_next;
												free(currNode);
												currNode = nextNode;
												q->m_size--;
								}
				}
}
