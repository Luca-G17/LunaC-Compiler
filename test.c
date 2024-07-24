#include <stdio.h>

int add(float x, float y) {
    return x + y;
} 

int main() {
    float x = 3.4;
    float y = 2.9;
    add(x, y);
    x = (int) add(x, y);
}