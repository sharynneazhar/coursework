/**
*	@file : Sorts.hpp
*	@author : Sharynne Azhar
*	@date : 2015.10.26
*	@brief: Implementation file for templated sort class
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
    while (!isSorted(arr, size))
    {
        shuffle(arr, size);
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
void Sorts<T>::mergeSort(T arr[], int size)
{
    if (size > 1)
    {
        int mid = size / 2;

        T* a1 = arr + 0;
        T* a2 = arr + mid;

        mergeSort(a1, mid);
        mergeSort(a2, size - mid);

        merge(a1, a2, mid, size - mid);
    }

    assert(Sorts<T>::isSorted(arr, size));
}

template <typename T>
void Sorts<T>::quickSort(T arr[], int size)
{
    quickSortRec(arr, 0, size - 1, false);
    assert(Sorts<T>::isSorted(arr, size));
}

template <typename T>
void Sorts<T>::quickSortWithMedian(T arr[], int size)
{
    quickSortRec(arr, 0, size - 1, true);
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

template <typename T>
void Sorts<T>::merge(T* a1, T* a2, int size1, int size2)
{
    T* arr = new int[size1 + size2]; // temporary array

    int firstIndexOfArr1 = 0;
    int lastIndexOfArr1 = size1 - 1;

    int firstIndexOfArr2 = 0;
    int lastIndexOfArr2 = size2 - 1;

    int index = 0;
    while ((firstIndexOfArr1 <= lastIndexOfArr1) && (firstIndexOfArr2 <= lastIndexOfArr2))
    {
        if (a1[firstIndexOfArr1] <= a2[firstIndexOfArr2])
        {
            arr[index] = a1[firstIndexOfArr1];
            firstIndexOfArr1++;
        }
        else
        {
            arr[index] = a2[firstIndexOfArr2];
            firstIndexOfArr2++;
        }

        index++;
    }

    while(firstIndexOfArr1 <= lastIndexOfArr1)
    {
        arr[index] = a1[firstIndexOfArr1];
        firstIndexOfArr1++;
        index++;
    }

    while(firstIndexOfArr2 <= lastIndexOfArr2)
    {
        arr[index] = a2[firstIndexOfArr2];
        firstIndexOfArr2++;
        index++;
    }

    for (index = 0; index <= size1 + size2 - 1; index++)
    {
        a1[index] = arr[index];
    }

    delete [] arr;
    arr = nullptr;
}

template <typename T>
void Sorts<T>::quickSortRec(T arr[], int first, int last, bool median)
{
    if (first < last)
    {
        int pivot = partition(arr, first, last, median);
        quickSortRec(arr, first, pivot - 1, median);
        quickSortRec(arr, pivot + 1, last, median);
    }
}

template <typename T>
void Sorts<T>::setMedianPivot(T arr[], int first, int last)
{
    if (last - first == 1)
    {
        if (arr[first] > arr[last])
        {
            swap(arr, first, last);
        }
    }
    else if (last - first == 0)
    {
        return;
    }
    else
    {
        int mid = (last - first + 1) / 2;
        if ((arr[first] > arr[mid]) && (arr[first] > arr[last]))
        {
            swap(arr, first, last);
        }
        else
        {
            if ((arr[mid] > arr[first]) && (arr[mid] > arr[last]))
            {
                swap(arr, mid, last);
            }
        }
    }
}

template <typename T>
int Sorts<T>::partition(T arr[], int first, int last, bool median)
{
    if (median)
    {
        setMedianPivot(arr, first, last);
    }

    int pivot = last;
    int indexLeft = first;
    int indexRight = last - 1;

    bool done = false;
    while(!done)
    {
        while(arr[indexLeft] < arr[pivot])
        {
            indexLeft++;
        }

        while(arr[indexRight] > arr[pivot])
        {
            indexRight--;
        }

        if (indexLeft < indexRight)
        {
            swap(arr, indexLeft, indexRight);
            indexLeft++;
            indexRight--;
        }
        else
        {
            done = true;
        }
    }

    swap(arr, pivot, indexLeft);

    pivot = indexLeft;
    return pivot;
}

template <typename T>
void Sorts<T>::shuffle(T arr[], int size)
{
    std::default_random_engine generator(time(nullptr));
	std::uniform_int_distribution<int> distribution(0, size-1);

	for(int i = 0; i < size; i++)
    {
		swap(arr,i,distribution(generator));
	}
}

template<typename T>
void Sorts<T>::swap(T arr[], int a, int b)
{
	T temp = arr[a];
	arr[a] = arr[b];
	arr[b] = temp;
}
