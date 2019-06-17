int main() {
    int a[2][3];
    *(*a + 3) = 7;
    return a[1][0];
}
