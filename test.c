#include <stdio.h>

int add(float x, float y) {
    return x + y;
}

int main() {
    float x = 3.4;
    float y = 2.9;
    int z[] = { 5, 2, 3};
    add(x, y);
    x = (int) add(x, y);

    int w = *(z + 1);
    printf("%d\n", w);
    *(z + 1) = 3;

    *(z + add(x, y)) = 1;
}