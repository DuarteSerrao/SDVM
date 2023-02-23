#include <stdio.h>
#include <stdlib.h>
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
    int i, count;
    int *list = NULL;
    
    if (argc < 2 ) return -1; // nome ficheiro, array


    count = argc - 1;
    list = (int *)malloc(count * sizeof(int));

    for(i=0;i<count;i++)
       list[i] = atoi(argv[i+1]);

    bubblesort(list, count);

    printf("\nSorted list:\n"); // Display the sorted array
    for (i = 0; i < count; i++)
    {
        printf("%d ", list[i]);// we're using C , so demonstrate pointers :) 
    }
    return 0;
}