/*
* @author Sharynne Azhar
* @file winner.cpp
*/

// Program determines the winner of a race

#include <iostream>
#include <cmath>

using namespace std;

int main()
{
	int numRunners;
	int runner_id;
	
	int numLaps;
	int keeper1;
	int keeper2;
	int keeper2_totalTime = 0;

	int fastestTime = 9999; 
	int winner;
	
	cout << "Enter the number of runners: ";
	cin >> numRunners;

	cout << "Enter the number of laps in each race: ";
	cin >> numLaps;

	cout << endl;

	// Get each runner's race results
	for (int i = 0; i < numRunners; i++)
	{
		cout << "Enter racer ID: ";
		cin >> runner_id;

		cout << "Enter first timekeeper's result: ";
		cin >> keeper1;

		// Get each runner's lap results
		cout << "Enter second timekeeper's results: " << endl;
		for (int j = 0; j < numLaps; j++)
		{
			cout << "\tLap " << j + 1 << ": ";
			cin >> keeper2;
			keeper2_totalTime += keeper2; // sums the results from timekeeper 2
		}

		// Compare timekeeper's results
		if (keeper1 == keeper2_totalTime)
		{
			cout << "Both timekeepers agree." << endl << endl;
		}
		else
		{
			cout << "The judges differ by " << abs(keeper1 - keeper2_totalTime) << endl << endl;
		}

		// Determine winner
		if (keeper1 < fastestTime)
		{
			fastestTime = keeper1;
			winner = runner_id;
		}

		keeper2_totalTime = 0; // reset timekeeper 2 
	}

	cout << "The winner is runner number " << winner << " with a time of " << fastestTime  << " seconds." << endl;


	return 0;
}

/*
Enter the number of runners: 4
Enter the number of laps in each race: 2

Enter racer ID: 1111
Enter first timekeeper's result: 120
Enter second timekeeper's results:
	Lap 1: 60
	Lap 2: 61
The judges differ by 1

Enter racer ID: 2222
Enter first timekeeper's result: 118
Enter second timekeeper's results:
	Lap 1: 60
	Lap 2: 58
Both timekeepers agree.

Enter racer ID: 3333
Enter first timekeeper's result: 121
Enter second timekeeper's results:
	Lap 1: 59
	Lap 2: 65
The judges differ by 3

Enter racer ID: 4444
Enter first timekeeper's result: 119
Enter second timekeeper's results:
	Lap 1: 57
	Lap 2: 62
Both timekeepers agree.

The winner is runner number 2222 with a time of 118 seconds.
Press any key to continue . . .
*/