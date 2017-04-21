/**
 * Buddy Allocator
 *
 * For the list library usage, see http://www.mcs.anl.gov/~kazutomo/list/
 */

/**************************************************************************
 * Conditional Compilation Options
 **************************************************************************/
#define USE_DEBUG 0

/**************************************************************************
 * Included Files
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>

#include "buddy.h"
#include "list.h"

/**************************************************************************
 * Public Definitions
 **************************************************************************/
#define MIN_ORDER 12
#define MAX_ORDER 20

#define PAGE_SIZE (1 << MIN_ORDER)

/* page index to address */
#define PAGE_TO_ADDR(page_idx) (void *)((page_idx*PAGE_SIZE) + g_memory)

/* address to page index */
#define ADDR_TO_PAGE(addr) ((unsigned long)((void *)addr - \
				(void *)g_memory) / PAGE_SIZE)

/* find buddy address */
#define BUDDY_ADDR(addr, o) (void *)((((unsigned long)addr - \
				(unsigned long)g_memory) ^ (1 << o)) + (unsigned long)g_memory)

#if USE_DEBUG == 1
#  define PDEBUG(fmt, ...) \
	fprintf(stderr, "%s(), %s:%d: " fmt,			\
		__func__, __FILE__, __LINE__, ##__VA_ARGS__)
#  define IFDEBUG(x) x
#else
#  define PDEBUG(fmt, ...)
#  define IFDEBUG(x)
#endif

/**************************************************************************
 * Public Types
 **************************************************************************/
typedef struct {
				struct list_head list;   // a double linked list data structure
				int order;               // order size of the current block
				int index;							 // the index of the page
				char *address;           // the address of the page
} page_t;

/**************************************************************************
 * Global Variables
 **************************************************************************/
/* free lists */
struct list_head free_area[MAX_ORDER+1];

/* memory area */
char g_memory[1 << MAX_ORDER];

/* page structures */
page_t g_pages[(1 << MAX_ORDER)/PAGE_SIZE];

/**************************************************************************
 * Public Function Prototypes
 **************************************************************************/

/**************************************************************************
 * Local Functions
 **************************************************************************/

/**
 * Initialize the buddy system
 */
void buddy_init()
{
				// Convert number of pages
				int n_pages = (1 << MAX_ORDER) / PAGE_SIZE;

				// Initialize the page variables
				for (int i = 0; i < n_pages; i++) {
								g_pages[i].order = -1;
								g_pages[i].index = i;
								g_pages[i].address = PAGE_TO_ADDR(i);
				}

				// Everything starts off in one block, so just set to max order
				g_pages[0].order = MAX_ORDER;

				/* initialize freelist */
				for (int i = MIN_ORDER; i <= MAX_ORDER; i++) {
								INIT_LIST_HEAD(&free_area[i]);
				}

				/* add the entire memory as a freeblock */
				list_add(&g_pages[0].list, &free_area[MAX_ORDER]);
}

/**
 * Allocate a memory block.
 *
 * On a memory request, the allocator returns the head of a free-list of the
 * matching size (i.e., smallest block that satisfies the request). If the
 * free-list of the matching block size is empty, then a larger block size will
 * be selected. The selected (large) block is then splitted into two smaller
 * blocks. Among the two blocks, left block will be used for allocation or be
 * further splitted while the right block will be added to the appropriate
 * free-list.
 *
 * @param size size in bytes
 * @return memory block address
 */
void *buddy_alloc(int size)
{
				int order = MIN_ORDER;				  	// order needed to allocate memory
				page_t *left = NULL;              // holds the left side of the page
				page_t *right = NULL;             // holds the right sid of the page

				// First check if out of bounds
				if ((size > (1 << MAX_ORDER)) || (size <= 0)) {
								return NULL;
				}

				// Find the order based on the block size given
				while ((order <= MAX_ORDER) && ((1 << order) < size)) {
								order++;
				}

				// Iterate through free_area to find the next free block of the same
				// order. Otherwise, continue breaking the block down into chunks
				for (int i = order; i <= MAX_ORDER; i++) {
								if (!list_empty(&free_area[i])) {
												// If we found a matching size, return the address.
												// Otherwise, do the break down into smaller chunks
												if (i == order) {
																// Get the left block
																left = list_entry(free_area[i].next,
																									page_t,
																									list);

																// Delete the entry from list
																list_del(&(left->list));

																// Update the left block order
																left->order = order;

																// Return the address
																return PAGE_TO_ADDR(left->index);
												}

												// Recursively move to the next biggest block size
												// and get the left block
												int new_size = 1 << (order + 1);
												left = &g_pages[ADDR_TO_PAGE(buddy_alloc(new_size))];

												// Get the right block and add it back to the free-list
												right = &g_pages[left->index + (1<<order) / PAGE_SIZE];
												list_add(&(right->list), &free_area[order]);

												// Update the left block properties
												left->order = order;

												// Return the address
												return PAGE_TO_ADDR(left->index);
								}
				}

				// Unable to find a block big enough for the request
				return NULL;
}

/**
 * Free an allocated memory block.
 *
 * Whenever a block is freed, the allocator checks its buddy. If the buddy is
 * free as well, then the two buddies are combined to form a bigger block. This
 * process continues until one of the buddies is not free.
 *
 * @param addr memory block address to be freed
 */
void buddy_free(void *addr)
{
				// Get the page index
				int index = ADDR_TO_PAGE(addr);

				// Loop through all the blocks
				for (int order = g_pages[index].order; order <= MAX_ORDER; order++) {
								page_t *entry = NULL;                 // list entry
								struct list_head *traverser = NULL;   // list traverser

								// Get the buddy's address
								char *buddy = (char*) BUDDY_ADDR(addr, order);

								// Loop through the free_area list for buddy
								list_for_each(traverser, &free_area[order]) {
												// Get the page entry to check if it is a buddy
												entry = list_entry(traverser, page_t, list);

												// No buddy, break out
												if (entry == NULL) {
																break;
												}

												// Found a matching buddy, break out
												if ((char*) entry->address == buddy)
																break;
								}

								// If buddy is null or address not equal, return to free_area
								if ((entry == NULL) || ((char*) entry->address != buddy)) {
												g_pages[index].order = -1;
												list_add(&g_pages[index].list, &free_area[order]);
												return;
								}

								// Buddy found, so combine.
								// If the current block address is larger than buddy's address,
								// update it to smaller value
								if ((char*) addr > entry->address) {
												addr = entry->address;
												index = ADDR_TO_PAGE(addr);
								}

								// Remove block from free_area
								list_del(&(entry->list));
				}
}

/**
 * Print the buddy system status---order oriented
 *
 * print free pages in each order.
 */
void buddy_dump()
{
				int o;
				for (o = MIN_ORDER; o <= MAX_ORDER; o++) {
								struct list_head *pos;
								int cnt = 0;
								list_for_each(pos, &free_area[o]) {
												cnt++;
								}
								printf("%d:%dK ", cnt, (1 << o)/1024);
				}
				printf("\n");
}
