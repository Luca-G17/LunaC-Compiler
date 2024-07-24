#include <stdio.h>

float add(float x, float y) {
    return x + y;
}

int main() {
    float x = 2.34;
    float y = 0.78;
    x = (int) add(x, y);
    printf("%f\n", x);
}