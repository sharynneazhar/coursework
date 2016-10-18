/**
*	@file : Sorts.hpp
*	@author : Sharynne Azhar
*	@date : 2015.10.19
*	@brief: Implementation of templated sort class
*/

template <typename T>
void Sorts<T>::bubbleSort(T arr[], int size)
{
    for (int i = 0; i < size - 1; i++)
    {
        for (int j = 0 ; j < size - 1; j++)
        {
            if (arr[j] > arr[j + 1])
            {
                swap(arr, j, (j + 1));
            }
        }
    }
    assert(Sorts<T>::isSorted(arr, size));
}

template <typename T>
void Sorts<T>::bogoSort(T arr[], int size)
{
    std::default_random_engine generator(time(nullptr));

    while (!isSorted(arr, size))
    {
        shuffle(arr, size, generator);
    }
    assert(Sorts<T>::isSorted(arr, size));
}

template <typename T>
void Sorts<T>::insertionSort(T arr[], int size)
{
    int sortThis;
    for (int firstUnsorted = 1; firstUnsorted < size; firstUnsorted++)
    {
        sortThis = firstUnsorted;
        while ((arr[sortThis] < arr[sortThis - 1]) && (sortThis > 0))
        {
            swap(arr, sortThis, (sortThis - 1));
            sortThis--;
        }
    }
    assert(Sorts<T>::isSorted(arr, size));
}

template <typename T>
void Sorts<T>::selectionSort(T arr[], int size)
{
    for (int i = 0; i < size; i++)
    {
        int minIndex = i;
        for (int j = i; j < size; j++)
        {
            // find the minimum value in the array
            if (arr[minIndex] > arr[j])
            {
                swap(arr, minIndex, j);
            }
        }
    }
    assert(Sorts<T>::isSorted(arr, size));
}

template <typename T>
bool Sorts<T>::isSorted(T arr[], int size)
{
    // false if current value is bigger than next
    for (int i = 0; i < size - 1; i++)
    {
        if (arr[i] > arr[i + 1])
        {
            return false;
        }
    }

    return true;
}

template <typename T>
void Sorts<T>::shuffle(T arr[], int size, std::default_random_engine& generator)
{
    // create a uniform distribution over the values of indices
	std::uniform_int_distribution<int> distribution(0, size-1);

	for(int i = 0; i < size; i++)
    {
        // swap each index with another at random
        int randomNumber = distribution(generator);
        swap(arr, i, randomNumber);
	}
}

template <typename T>
int* Sorts<T>::createTestArray(int size, int min, int max)
{
    // seeds a random engine with time
    std::default_random_engine generator(time(nullptr));

    // uniform distribution of expected values
	std::uniform_int_distribution<int> distribution(min, max);

	int* arr = new int [size];
	for(int i = 0; i < size; i++)
    {
        // grab a number from the distribution using the generator
        arr[i] = distribution(generator);
	}

	return arr;
}

template <typename T>
double Sorts<T>::sortTimer(std::function<void(T[],int)> sort, T arr[], int size)
{
    std::chrono::system_clock::time_point start; // starting time
    std::chrono::system_clock::time_point end; // ending time
    std::chrono::duration<double> elapsed; // time elapsed

    start = std::chrono::system_clock::now(); // current time
    sort(arr, size);

    end = std::chrono::system_clock::now(); // current time

    elapsed = (end-start); // difference time elapsed
    return (elapsed.count()); // return converted to seconds
}

template<typename T>
void Sorts<T>::swap(T arr[], int a, int b)
{
	T temp = arr[a];
	arr[a] = arr[b];
	arr[b] = temp;
}
