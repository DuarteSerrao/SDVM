#include<stdio.h>

void quicksort(int number[25],int first,int last){
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

int main(int argc, char** argv){
    int i, count;
    int *list = NULL;
    
    if (argc < 2 || argc < (2+argv[1])) return -1; // nome ficheiro, tamanho, array


    count = argv[1];
    list = (int *)malloc(n * sizeof(int));
 
    quicksort(list,0,count-1);
 
    printf("Sorted elements: ");
    for(i=0;i<count;i++)
       printf(" %d",list[i]);
 
    return 0;
}