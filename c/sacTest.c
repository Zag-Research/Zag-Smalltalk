#include <stdint.h>
#include <stdio.h>
#include <math.h>
#include "halfsiphash.h"

double fact(int n) {
    double result = 1.0;
    for (int i = 2; i <= n; i++)
        result *= i;
    return result;
}
double prob(int n, int k) {
    return (fact(n) / fact(k) / fact(n - k)) * pow(0.5, (double)n);
}
/*
From: "The strict avalanche criterion randomness test"

    February 2005Mathematics and Computers in Simulation 68(1):1-7

DOI:10.1016/j.matcom.2004.09.001
*/
double SAC_test (uint32_t (*rng)(uint32_t), int order) {
    const uint32_t m = 10000;
    uint32_t n, i, j;
    uint32_t xor, xor32, alea[m+1];
    int eacount[33];
    int hamming = 0;
    double expected = 0.0, suma = 0.0;
    double chi = 0.0;
    for (i = 0; i < 33; i++)
        eacount[i] = 0;
    for (i = 0; i <= m; ++i)
        alea[i] = rng(i);
    for (i = 0; i < m; ++i){
        hamming = 0;
        xor32 = alea[i] ^ alea[i + 1];
        xor = xor32;
        for(j = 0; j < 32; j++) {
            if (xor & 1)
                hamming++;
            xor = xor >> 1;
            if ((j % order) == order-1) {
                eacount[hamming]++;
                hamming = 0;
            }
        }
    }
    chi = 0.0;
    for (i = 0; i <= order; i++){
        expected = (32/order*m) * prob(order,i);
        if (expected > 5.0)
            suma = (expected-eacount[i])*(expected-eacount[i])/expected;
        else suma = 0;
        chi = chi+suma;
    }
    return chi;
}
/* From: https://nullprogram.com/blog/2018/07/31/ */
uint32_t triple32(uint32_t x) {
    x ^= x >> 17;
    x *= 0xed5ad4bbU;
    x ^= x >> 11;
    x *= 0xac4c1b51U;
    x ^= x >> 15;
    x *= 0x31848babU;
    x ^= x >> 14;
    return x;
}
uint32_t inversePhi32(uint32_t x) {
    return x * 2654435769U;
}
uint32_t key[2]= {
    0xdeadbeef,
    0xdeadbeef
};
uint32_t doHalfSipHash(uint32_t x) {
    uint32_t in = x, out;
    halfsiphash(&in, sizeof(in), key, &out, sizeof(out));
    return out;
}
int main() {
    /* Significance level:
     * d.o.f.  0.05   0.01
     * 8      15.50  20.09
     * 16     26.29  32.00
     * 32     46.19  53.48
     * 64     83.67  93.21
     */
    printf("'triple32' has a chi-square-8 value of %3.9f\n",SAC_test(triple32, 8));
    printf("'triple32' has a chi-square-16 value of %3.9f\n",SAC_test(triple32, 16));
    printf("'triple32' has a chi-square-32 value of %3.9f\n",SAC_test(triple32, 32));
    printf("'doHalfSipHash' has a chi-square-8 value of %3.9f\n",SAC_test(doHalfSipHash, 8));
    printf("'doHalfSipHash' has a chi-square-16 value of %3.9f\n",SAC_test(doHalfSipHash, 16));
    printf("'doHalfSipHash' has a chi-square-32 value of %3.9f\n",SAC_test(doHalfSipHash, 32));
    printf("'inversePhi32' has a chi-square-8 value of %3.9f\n",SAC_test(inversePhi32, 8));
    printf("'inversePhi32' has a chi-square-16 value of %3.9f\n",SAC_test(inversePhi32, 16));
    printf("'inversePhi32' has a chi-square-32 value of %3.9f\n",SAC_test(inversePhi32, 32));
    return 0;
}
