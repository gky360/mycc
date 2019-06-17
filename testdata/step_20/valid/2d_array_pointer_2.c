int main() {
    int a[2][3];
    *(*a + 5) = 7;
    return a[1][2];
}
