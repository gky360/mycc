void alloc4(int **p, int n0, int n1, int n2, int n3) {
    *p = malloc(4 * sizeof(int));
    p[0] = n0;
    p[1] = n1;
    p[2] = n2;
    p[3] = n3;
}