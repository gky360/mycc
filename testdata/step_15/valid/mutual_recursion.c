foo(a);
bar(b);

main() {
    return foo(5);
}

foo(a) {
    if (a <= 0) {
        return a;
    }

    return a + bar(a - 1);
}

bar(b) {
    if (b <= 0) {
        return b;
    }

    return b + bar(b / 2);
}