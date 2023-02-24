public class QuickSort {

    static int[] arr = { 53, 30, 75, 5, 86, 66, 22, 12, 71, 17, 121, 119, 93, 55, 10, 69, 78, 16, 94, 102, 58, 81, 2,
            34, 105, 114, 31, 8, 91, 65, 110, 111, 19, 109, 74, 51, 36, 61, 33, 96, 107, 21, 20, 101, 50, 39, 76, 64,
            68, 18, 90,
            14, 54, 108, 48, 100, 89, 63, 6, 87, 72, 113, 43, 97, 84, 117, 32, 124, 1, 37, 29, 126, 3, 122, 118, 57, 44,
            52, 116, 70, 56, 15, 7, 125, 92, 62, 99, 67, 25, 60, 38, 98, 45, 77, 128, 46, 83, 9, 106, 42, 80, 26, 24,
            23, 104, 103, 123, 40, 82, 79, 127, 27, 88, 4, 13, 28, 120, 95, 112, 47, 85, 35, 41, 115, 11, 73, 59, 49 };

    static int n = 128;

    public static void main(String args[]) {
        quickSort(arr, 0, n - 1);

        System.out.println("\nThe sorted array : ;");
        for (int i = 0; i < n; i++)
            System.out.print(arr[i] + " ");
        System.out.println();
    }

    /**
     * @param arr   The array/sub-array to be sorted
     * @param lower lower index
     * @param upper upper index
     */
    static void quickSort(int arr[], int lower, int upper) {
        if (lower >= upper)
            return;
        int p = partition(arr, lower, upper);
        quickSort(arr, lower, p - 1);
        quickSort(arr, p + 1, upper);
    }

    /**
     * @param arr   The array of elements
     * @param lower The lower index of the elements
     * @param upper The upper index of the elements
     * @return The index of the pivot element
     */
    private static int partition(int arr[], int lower, int upper) {
        int pivot = arr[upper];
        int j = lower;
        int tmp;
        for (int i = lower; i <= upper; i++) {
            if (arr[i] < pivot) {
                tmp = arr[i];
                arr[i] = arr[j];
                arr[j] = tmp;
                j++;
            }
        }
        tmp = arr[upper];
        arr[upper] = arr[j];
        arr[j] = tmp;

        return j;
    }
}