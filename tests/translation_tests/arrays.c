#include <stdio.h>

int arr_func_test(int x[2][2][2]) {
    x[1][0][1] += 10;
    return x[1][0][1];
}

int main() {
    int res_0 = 0;
    int res_1 = 0;
    int res_2 = 0;
    float res_3 = 0;

    int x[2][2][2] = { { { 1, 4 }, { 3, 5 } }, { { 3, 1 }, {4, 8} } };
    res_0 = x[1][1][0];
    printf("%d\n", res_0); // 4

    res_1 = arr_func_test(x);
    printf("%d\n", res_1); // 11
    
    x[0][0][0] += x[0][0][1];
    res_2 = x[0][0][0];
    printf("%d\n", res_2); // 5

    float f1 = 12.1;
    float f2 = 16.9;
    float* f_ptr_array[2];
    f_ptr_array[0] = &f1;
    f_ptr_array[1] = &f2;
    res_3 = *(f_ptr_array[0]) + *(f_ptr_array[1]);
    printf("%f\n", res_3);
}