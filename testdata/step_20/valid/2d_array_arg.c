int foo(int a[2][3]) { return a[1][1]; }

int main() {
    int a[2][3];
    a[1][1] = 7;
    return foo(a);
}
