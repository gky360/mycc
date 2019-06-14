void alloc4(int **p, int n0, int n1, int n2, int n3) {
    int *arr = malloc(4 * sizeof(int));
    arr[0] = n0;
    arr[1] = n1;
    arr[2] = n2;
    arr[3] = n3;
    *p = arr;
}