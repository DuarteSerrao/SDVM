#include <stdio.h>
#include <stdlib.h>

const int count = 128;
int list[128] = {53, 30, 75, 5, 86, 66, 22, 12, 71, 17, 121, 119, 93, 55, 10, 69, 78, 16, 94, 102, 58, 81, 2, 34, 105, 114, 31, 8, 91, 65, 110, 111, 19, 109, 74, 51, 36, 61, 33, 96, 107, 21, 20, 101, 50, 39, 76, 64, 68, 18, 90, 14, 54, 108, 48, 100, 89, 63, 6, 87, 72, 113, 43, 97, 84, 117, 32, 124, 1, 37, 29, 126, 3, 122, 118, 57, 44, 52, 116, 70, 56, 15, 7, 125, 92, 62, 99, 67, 25, 60, 38, 98, 45, 77, 128, 46, 83, 9, 106, 42, 80, 26, 24, 23, 104, 103, 123, 40, 82, 79, 127, 27, 88, 4, 13, 28, 120, 95, 112, 47, 85, 35, 41, 115, 11, 73, 59, 49};


int swapped = 0; // global variable to check if swap() function is called
void swap(int *a, int *b)
{
    int temp = *b;
    *b = *a;
    *a = temp;
    swapped++;
}
int bubblesort(int *a, int size) // to demonstrate passing by refference in C (pointer variable receives only the base address of array)
{
    for (int i = 0; i < size; i++)
    {
        for (int j = 0; j < size - i - 1; j++)
        {
            if (a[j] > a[j + 1])
            {
            swap(a+j, a+j+1); // making a function is good instead of many lines of code in main function 
            }
        }
        if (swapped == 0)
            return 1; // use return values better than break statement
    }
    return 0; // sorted 

}

int main(int argc, char** argv)
{
    int i;

    bubblesort(list, count);

    printf("\nSorted list:\n"); // Display the sorted array
    for (i = 0; i < count; i++)
    {
        printf("%d ", list[i]);// we're using C , so demonstrate pointers :) 
    }
    return 0;
}