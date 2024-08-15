#include <stdio.h>

int poo2(int x[2][4]) {
    x[1][2] = -12;
}

int test(int x[2][4]) {
    x[1][1] += 4;
    return x[1][0];
}

int main() {
    int y = 0;
    int z = 0;
    int x[2][4] = { {3, 2, 1, 7}, {3, 10, 8, 0 } };
    poo2(x);
    z = test(x);
    x[1][0] += 2;
    y = x[1][0];
}