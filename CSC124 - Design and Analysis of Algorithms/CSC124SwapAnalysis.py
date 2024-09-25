'''
Ramel Cary B. Jamen - 2019-2093
CSC124 - Programming and Time Analysis
Referrences:
	https://www.geeksforgeeks.org/bubble-sort/
	https://www.geeksforgeeks.org/insertion-sort/?ref=shm
	https://www.geeksforgeeks.org/selection-sort/?ref=shm
	https://www.geeksforgeeks.org/quick-sort/?ref=shm

	For Testing: print(f"After swap {count}: {arr}")
'''

def driver(A):
	print('Array Unsorted:\n',A)
	bSort, bCount = bubbleSort(A.copy())
	iSort, iCount = insertionSort(A.copy())
	sSort, sCount = selectionSort(A.copy())
	pSort, pCount = partitionSort(A.copy())
	print('\nBubble Sort Sorted Array:\n',bSort,'\nBubble Swap Count:', bCount)
	print('\nInsertion Sort Sorted Array:\n',iSort,'\nInsertion Swap Count:', iCount)
	print('\nSelection Sort Sorted Array:\n',sSort,'\nSelection Swap Count:', sCount)
	print('\nPartition Sort Sorted Array:\n',pSort,'\nPartition Swap Count:', pCount)

def swap(A, i, j):
	A[i], A[j] = A[j], A[i]

def bubbleSort(arr):
	length = len(arr)
	count = 0
	i = 0
	while i < length - 1:
		j = 0
		while j < length - i - 1:
			if arr[j] > arr[j + 1]:
				swap(arr, j, j + 1)
				count += 1
			j += 1
		i += 1
	return arr, count - 1

def insertionSort(arr):
    length = len(arr)
    count = 0
    i = 1
    while i < length:
        key = arr[i]
        j = i - 1
        while j >= 0 and arr[j] > key:
            swap(arr, j, j + 1)
            count += 1
            j -= 1
        i += 1
    return arr, count - 1

def selectionSort(arr):
    length = len(arr)
    count = 0
    i = 0
    while i < length:
        min_index = i
        j = i + 1
        while j < length:
            if arr[j] < arr[min_index]:
                min_index = j
            j += 1
        swap(arr, i, min_index)
        count += 1
        i += 1
    return arr, count - 1

def partitionSort(arr):
	length = len(arr)
	count = 0
	def quicksort(arr, low, high):
		if low < high:
			pi = partition(arr, low, high)
			quicksort(arr, low, pi - 1)
			quicksort(arr, pi + 1, high)

	def partition(arr, low, high):
		nonlocal count
		pivot = arr[high]
		i = low - 1
		j = low
		while j < high:
			if arr[j] < pivot:
				i += 1
				swap(arr, i, j)
				count += 1
			j += 1
		swap(arr, i + 1, high)
		count += 1
		return i + 1

	quicksort(arr, 0, length - 1)
	return arr, count


if __name__ == "__main__":
	#Array
	A = ['r','a','m','e','l',' ','c','a','r','y',' ','j','a','m','e','n']
	#Driver Code for Sorting
	driver(A)
