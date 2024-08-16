#include <stdio.h>

int test(int x[2][4]) {
    x[1][1] += 4;
    return x[1][0];
}

int main() {
    int x = 0;
    int y = 1;
    int z = 10;
    int* a[10];
    a[0] = &y;
    a[1] = &z;
    x = *a[0] + *a[1];
}