#include <stdio.h>

int add(int x, int y) {
    return x + y;
}

int add_f(float x, float y) {
    return x + y;
}

float add_f_ret_f(float x, float y) {
    return x + y;
}

int main() {
    int res_1;
    int res_2;
    float res_3;
    int res_4;

    float x = 2.5;
    float y = 2.9;

    res_1 = add(x, y);
    res_2 = add_f(x, y);
    res_3 = add_f_ret_f(x, y);
    res_4 = add_f_ret_f(x, y);
    printf("%d\n", res_1);
    printf("%d\n", res_2);
    printf("%f\n", res_3);
    printf("%d\n", res_4);
}