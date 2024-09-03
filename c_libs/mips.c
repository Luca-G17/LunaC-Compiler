#include "mips.h"
#include <math.h>
#include <stdlib.h>
#include <unistd.h>

float m_sin(float a) {
    return sinf(a);
}

float m_cos(float a) {
    return cosf(a);
}

float m_tan(float a) {
    return tanf(a);
}

float m_asin(float a) {
    return asinf(a);
}

float m_acos(float a) {
    return acosf(a);
}

float m_atan(float a) {
    return atanf(a);
}

float m_atan2(float a, float b) {
    return atan2f(a, b);
}

float m_abs(float a) {
    return fabsf(a);
}

float m_ceil(float a) {
    return ceilf(a);
}

float m_floor(float a) {
    return floor(a);
}

float m_trunc(float a) {
    return truncf(a);
}

float m_round(float a) {
    return roundf(a);
}

float m_min(float a, float b) {
    return fminf(a, b);
}

float m_max(float a, float b) {
    return fmaxf(a, b);
}

float m_exp(float a) {
    return expf(a);
}

float m_log(float a) {
    return logf(a);
}

float m_sqrt(float a) {
    return sqrtf(a);
}

float m_rand(float a) {
    return (float)rand() / ((float) RAND_MAX);
}

void m_sleep(float a) {
    sleep(a);
}

void m_yeild() {
    sleep(1.0/2.0);
}