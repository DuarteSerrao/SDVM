#include<stdio.h>


const int count = 512;
int list[512] = {476, 140, 500, 387, 477, 99, 63, 159, 104, 26, 237, 381, 87, 377, 339, 82, 160, 300, 129, 277, 362, 40, 62, 39, 84, 227, 136, 297, 470, 493, 235, 314, 396, 306, 189, 318, 366, 298, 48, 421, 349, 364, 264, 498, 35, 400, 192, 407, 475, 501, 96, 114, 255, 361, 441, 56, 420, 220, 449, 367, 497, 91, 332, 384, 77, 112, 352, 479, 173, 460, 285, 487, 12, 340, 83, 417, 254, 360, 27, 347, 242, 9, 503, 317, 388, 157, 304, 437, 175, 428, 105, 1, 307, 64, 357, 469, 267, 316, 194, 369, 18, 123, 186, 259, 15, 395, 19, 291, 67, 358, 302, 88, 311, 293, 413, 231, 146, 205, 229, 122, 151, 17, 435, 313, 208, 337, 115, 438, 29, 76, 271, 508, 336, 14, 409, 283, 47, 330, 457, 13, 454, 36, 245, 191, 10, 374, 44, 466, 3, 444, 353, 224, 130, 326, 496, 142, 143, 315, 154, 289, 236, 474, 399, 170, 346, 463, 379, 166, 57, 95, 246, 145, 299, 424, 156, 188, 22, 335, 223, 108, 252, 459, 410, 249, 230, 11, 37, 128, 161, 4, 45, 423, 263, 239, 270, 184, 465, 211, 167, 228, 150, 164, 178, 294, 234, 436, 183, 106, 182, 492, 495, 482, 422, 185, 109, 344, 426, 451, 334, 198, 506, 6, 301, 148, 93, 53, 30, 348, 512, 275, 430, 174, 308, 276, 305, 433, 350, 141, 439, 233, 258, 354, 42, 505, 342, 215, 440, 217, 327, 120, 51, 21, 152, 309, 485, 137, 86, 312, 34, 125, 287, 250, 222, 443, 59, 455, 144, 195, 110, 251, 321, 406, 138, 329, 134, 450, 414, 401, 345, 117, 65, 452, 121, 24, 278, 193, 260, 163, 403, 118, 378, 510, 481, 386, 158, 499, 213, 292, 370, 402, 247, 31, 131, 74, 320, 111, 290, 391, 262, 373, 494, 107, 256, 187, 461, 49, 133, 139, 310, 504, 8, 23, 16, 200, 398, 79, 172, 207, 94, 397, 72, 155, 380, 190, 97, 412, 434, 480, 319, 206, 179, 393, 199, 468, 265, 61, 165, 202, 33, 212, 149, 168, 203, 453, 288, 284, 324, 359, 135, 101, 296, 464, 210, 100, 280, 405, 486, 323, 68, 221, 442, 363, 248, 46, 73, 269, 41, 116, 328, 181, 483, 404, 103, 282, 419, 81, 351, 416, 32, 209, 119, 126, 392, 365, 368, 177, 268, 238, 55, 338, 415, 71, 102, 429, 20, 507, 218, 281, 66, 216, 432, 226, 58, 489, 132, 38, 78, 372, 385, 214, 257, 445, 331, 54, 153, 92, 471, 322, 69, 511, 411, 502, 371, 427, 273, 355, 266, 376, 25, 176, 98, 408, 50, 180, 201, 333, 169, 147, 303, 127, 80, 295, 124, 425, 7, 261, 90, 241, 375, 60, 232, 325, 458, 286, 85, 448, 244, 171, 382, 491, 390, 447, 219, 197, 389, 196, 394, 253, 225, 356, 488, 484, 28, 473, 456, 43, 467, 89, 431, 418, 478, 509, 343, 5, 341, 383, 446, 240, 274, 472, 2, 272, 52, 75, 462, 243, 70, 113, 279, 162, 204, 490};

void quicksort(int number[],int first,int last){
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