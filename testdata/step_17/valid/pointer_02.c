int main() {
    int x = 1;
    int *y = &x;
    *y = 3;
    return x;
}