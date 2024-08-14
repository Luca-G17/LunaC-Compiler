#include <stdio.h>



int main() {
    int y = 0;
    int x[2][2][2] = { { { 1, 4 }, { 3, 5 } }, { { 3, 1 }, {4, 8} } };
    y = x[1][0][1];
}