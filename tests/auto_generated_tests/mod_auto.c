#include <stdio.h>
int main() {
    int x = 5;
    x %= 4;
    x = x % 4;
    printf("%d\n", x);
}