/*
* @author Sharynne Azhar
* @file Proj2_ske.cpp
*/

//       CS 200   Project #2
//          --  complete the following program
//          --  test the program
//                    -- using keyboard input
//                    -- letting the computer generate the input

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <iomanip>

using namespace std;

const bool   KEYBOARD = false;
const double TAX_RATE = 0.0725; 

ofstream Out("con");

int    getNumberOfSales(void);
int    getItemNumber(void);
double getPrice(int itemNumber);
bool   isTaxable(int itemNumber);
int    getQuantity(int itemNumber);
double getCost(int itemNumber, int count, double price);
double getTax(double sales);
void   printLine(ostream &w, int iNumber, int qty, double price, double cost, bool taxable);
void   printTotal(ostream &w, int loopCount, double grandTotal, double taxDue);
void   headings(ostream &w);
void   startRandom(void);
void   prepareOutput(ostream &w);

void main()
{
	int differentItems,       // Number of items to purchase
		iNumber,              // Item number code
		qty;                  // Number of a particular item purchased
	double price,             // Price of a particular item
		cost,                 // The cost of the item purchase = price * qty
		taxableTotal = 0,         // Total of all taxable purchases
		nonTaxableTotal = 0,      // Total of all nontaxable purchases
		taxDue,               // Tax due on the taxable total
		grandTotal = 0;           // Sum of taxableTotal, nonTaxableTotal
	bool   taxable;           // Flag to indicate if the item is taxable

	prepareOutput(Out);
	
	if (!KEYBOARD) // for computer generate
	{
		startRandom();
		headings(Out);  
	}

	differentItems = getNumberOfSales();
	
	for (int i = 0; i < differentItems; i++)
	{
		iNumber = getItemNumber();
		qty = getQuantity(iNumber);
		price = getPrice(iNumber);
		cost = getCost(iNumber, qty, price);
		taxable = isTaxable(iNumber);
		
		if (KEYBOARD) // for keyboard input
		{
			prepareOutput(cout); 
			headings(cout); 
			printLine(cout, iNumber, qty, price, cost, taxable);
			cout << endl;
		}
		else
		{
			printLine(Out, iNumber, qty, price, cost, taxable);
		}
	
		if (taxable)
		{
			taxableTotal += cost;
		}
		else
		{
			nonTaxableTotal += cost;
		}
	}  // end for

	grandTotal += taxableTotal + nonTaxableTotal;
	taxDue = getTax(taxableTotal);
	printTotal(Out, differentItems, grandTotal, taxDue);
}  // end main


void startRandom(void)
{
	int seed;
	cout << "Enter seed value for random number generator: ";
	cin >> seed;
	srand(seed);
}


int getItemNumber(void)
{                             //  item number should be a 4-digit integer
	int num;

	if (KEYBOARD)
	{
		cout << "Enter item number: ";
		cin >> num;
	}
	else
		num = rand() % 9000 + 1000;

	return num;
}

double getPrice(int num)
{                            //  price should be between .10 and 10.09
	double price;
	
	if (KEYBOARD)
	{
		cout << "Enter price for item  " << num << " : ";
		cin >> price;
	}
	else
		price = double(rand() % 1000 + 10) / 100;
	
	return price;

}
bool isTaxable(int itemNumber)
{
	char answer;

	if (KEYBOARD) // ask the user
	{
		cout << "Is this item number " << itemNumber << " taxable (y or n): ";
		cin >> answer;
		
		if (answer == 'y')
			return true;
		else
			return false;
	}
	else //   computer will make NOT taxable if itemNumer is divisible by 5
	{
		if (itemNumber % 5 == 0)
			return false;
		else
			return true;
	}
}
int getQuantity(int num)
{
	int quantity; 

	if (KEYBOARD) // ask the user
	{
		cout << "How many of item number " << num << " were purchased?: ";
		cin >> quantity;
		return quantity;
	}
	else //   computer will make a choice between 1 and 8
	{
		return (rand() % 8 + 1);
	}
}
int getNumberOfSales(void)
{
	int saleNum;

	if (KEYBOARD) //  ask the user
	{
		cout << "How many different items to purchase? ";
		cin >> saleNum;
		return saleNum;
	}
	else //   computer will make a choice between 1 and 15
	{
		return (rand() % 15 + 1);
	}
}
double getCost(int itemNumber, int count, double price)
{
	return (count * price);
}

double getTax(double sales)
{  //   define a const for the sales tax rate  - USE a rate of  0.0725
	return (sales * TAX_RATE); 
}

void printLine(ostream &w, int iNumber, int qty, double price, double cost, bool taxable)
{
	w << endl << iNumber << setw(10) << qty << setw(13) << price << setw(11) << cost;
	
	if (!taxable)
	{
		w << setw(2) << "*";  // print a "*" for the item which is
		// not taxable
	}
}

void printTotal(ostream &w, int loopCount, double grandTotal, double taxDue)
{
	w << endl << endl;
	w << "Purchase: " << grandTotal << endl;
	w << "Tax:      " << taxDue << endl; 
	w << "Amount:   " << grandTotal + taxDue << endl << endl;
	
	w << loopCount << " different kinds of items purchased" << endl;

	w << endl << " * indicates item was not taxable ." << endl;
}

void headings(ostream &w)
{
	w << "Item #" << setw(12) << "Quantity" << setw(10) << "Price"
		<< setw(10) << "Cost" << endl;
}

void prepareOutput(ostream &w)
{
	w << setiosflags(ios::showpoint | ios::fixed) << setprecision(2);
}



/*----------COMPUTER GENERATES INPUTS
Enter seed value for random number generator: 7750
item #  quantity     price      cost

3613         8      4.40     35.20
8655         2      6.22     12.44  *
4361         4      7.69     30.76
7460         7      1.91     13.37  *
5124         5      5.67     28.35
1967         2      5.37     10.74
1722         5      7.53     37.65
1396         3      8.45     25.35
1555         8      6.76     54.08  *
7401         7      2.65     18.55
4125         4      9.06     36.24  *
2800         5      4.22     21.10  *


Purchase:   323.83
Tax:         13.53
Amount:     337.36

12 kinds of different items purchased

* indicates item was not taxable
Press any key to continue
*/

/*-----------KEYBOARD INPUTS
How many different items to purchase? 2
Enter item number: 1234
How many of item number  1234 were purchased?: 3
Enter price for item  1234 : 1.7
Is item number  1234 taxable ( y or n) : y
item #  quantity     price      cost

1234         3      1.70      5.10
Enter item number: 1235
How many of item number  1235 were purchased?: 3
Enter price for item  1235 : 1.9
Is item number  1235 taxable ( y or n) : n
item #  quantity     price      cost

1235         3      1.90      5.70  *


Purchase:    10.80
Tax:          0.37
Amount:      11.17

2 kinds of different items purchased

* indicates item was not taxable
Press any key to continue
*/


