int foo() {
    return 1;
}

int main() {
    int a[3];
    a[2] = 5;
    return a[foo() + 1];
}