int main() {
    int a[3];
    *a = 1;
    *(a + 1) = 2;
    a[2] = 4;
    int *p;
    p = a;
    return p[0] + p[1] + *(p + 2);
}