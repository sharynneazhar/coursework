/**
* @author Sharynne Azhar
* @file inventory.cpp
*/

#include "inventory.h"

inventory::inventory()
{
	itemId = 0;
	storeNr = 0;
	quantity = 0;
}

inventory::inventory(int id, int store, int qt)
{
	itemId = id;
	storeNr = store;
	quantity = qt;
}

void inventory::setId(int id)
{
	itemId = id;
}

void inventory::setStoreNr(int store)
{
	storeNr = store;
}

void inventory::setQuantity(int qt)
{
	quantity = qt;
}

int inventory::getId(void) const
{
	return itemId;
}

int inventory::getStoreNr(void) const
{
	return storeNr;
}

int inventory::getQuantity(void) const
{
	return quantity;
}