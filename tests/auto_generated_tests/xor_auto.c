#include <stdio.h>
int main() {
    int x = 4;
    x ^= 2;
    x = x ^ 2;
    printf("%d\n", x);
}
