#include <stdio.h>
int main() {
    int x = 5;
    x |= 2;
    x = x | 2;
    printf("%d\n", x);
}
