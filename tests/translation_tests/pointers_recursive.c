int inc(int* x) {
    if (*x > 10) {
        return 0;
    }
    *x += 1;
    inc(x);
}

int main() {
    int x = 0;
    inc(&x);
}