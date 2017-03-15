/** @file libpriqueue.h
 */

#ifndef LIBPRIQUEUE_H_
#define LIBPRIQUEUE_H_

/**
   Node Data Structure
 */
typedef struct node_t node_t;
struct node_t
{
        void *m_ptr;  /**< Pointer to value stored in the node object */
        node_t *m_next; /**< Pointer to the next node in queue */
};

/**
   Priqueue Data Structure
 */
typedef struct _priqueue_t
{
        node_t *m_head;    /**< Pointer to the first node of the queue */
        size_t m_size;      /**< Size of the queue */
        int (*m_comparer)(const void *, const void*); /**< Compare function */
} priqueue_t;


void   priqueue_init     (priqueue_t *q, int (*comparer)(const void *, const void *));

int    priqueue_offer    (priqueue_t *q, void *ptr);
void * priqueue_peek     (priqueue_t *q);
void * priqueue_poll     (priqueue_t *q);
void * priqueue_at       (priqueue_t *q, int index);
int    priqueue_remove   (priqueue_t *q, void *ptr);
void * priqueue_remove_at(priqueue_t *q, int index);
int    priqueue_size     (priqueue_t *q);

void   priqueue_destroy  (priqueue_t *q);

#endif /* LIBPQUEUE_H_ */
