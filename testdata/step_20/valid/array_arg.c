int foo(int a[3]) { return a[1]; }

int main() {
    int a[3];
    a[0] = 10;
    a[1] = 11;
    a[2] = 12;
    return foo(a);
}
