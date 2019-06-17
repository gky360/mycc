int main() {
    int a[2][3];
    *(*(a + 1) + 2) = 9;
    return a[1][2];
}
