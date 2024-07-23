#include <stdio.h>

int add(int x, int y) {
    return x + y;
} 

int main() {
    float x = 3.4;
    float y = 2.9;
    add(x, y);
    x = add(x, y) + 0.5;
}