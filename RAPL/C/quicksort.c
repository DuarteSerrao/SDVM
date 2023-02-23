#include<stdio.h>


int count = 128;
int list[count] = {123, 45, 78, 34, 102, 121, 37, 49, 36, 12, 40, 76, 65, 44, 111, 128, 3, 69, 118, 35, 125, 112, 88, 20, 92, 53, 30, 90, 29, 124, 115, 54, 95, 70, 97, 122, 99, 113, 105, 110, 75, 50, 127, 82, 19, 22, 109, 33, 6, 31, 85, 87, 83, 64, 24, 93, 42, 91, 4, 98, 116, 25, 117, 107, 126, 10, 77, 46, 15, 59, 48, 60, 2, 17, 86, 58, 80, 51, 119, 61, 9, 56, 74, 43, 14, 89, 11, 71, 23, 100, 114, 96, 84, 7, 27, 79, 1, 72, 28, 16, 108, 106, 41, 52, 32, 47, 73, 62, 66, 104, 18, 8, 101, 13, 39, 55, 57, 120, 68, 5, 103, 94, 26, 38, 21, 67, 63, 81}

void quicksort(int number[count],int first,int last){
    int i, j, pivot, temp;

    if(first<last){
        pivot=first;
        i=first;
        j=last;

        while(i<j){
            while(number[i]<=number[pivot]&&i<last)
                i++;
            while(number[j]>number[pivot])
                j--;
            if(i<j){
                temp=number[i];
                number[i]=number[j];
                number[j]=temp;
            }
        }

        temp=number[pivot];
        number[pivot]=number[j];
        number[j]=temp;
        quicksort(number,first,j-1);
        quicksort(number,j+1,last);

   }
}

int main(){
    int i;
 
    quicksort(list,0,count-1);
    
    printf("\nSorted list:\n"); // Display the sorted array
    for (i = 0; i < count; i++)
    {
        printf("%d ", list[i]);// we're using C , so demonstrate pointers :) 
    }

    return 0;
}