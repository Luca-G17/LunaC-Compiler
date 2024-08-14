#include <stdio.h>

int add(float x, float y) {
    return x + y;
}

int main() {
    int y = 0;
    int x[2][4] = { {3, 2, 1, 7}, {3, 10, 8, 0 } };
    //*(*(x + 1) + 1) = 4;
    x[1][1] = 4;
    y = x[1][1];
}