int main() {
    int *p;
    alloc4(&p, 1, 2, 4, 8);
    int *q;
    q = p + 2;
    q = 1 + q;
    return *q;
}