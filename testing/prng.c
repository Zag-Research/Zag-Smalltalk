/*
 * Experiments with Linear Congruential Generator to generate hash values for Symbols
 *   - unique values for all Symbols
 *   - use in perfect hash tables for method lookup
 *     https://en.wikipedia.org/wiki/Linear_congruential_generator
 */
#include <math.h>
#include <stdio.h>
#define NPrimes 20
#define MaxBuckets 8192
int primes[NPrimes];
int nPrimes=0;
unsigned samples[NPrimes];
unsigned counts[NPrimes][MaxBuckets];
int nextPrime(int n) {
  int isPrime;
  if ((n&1)==0) n++;
  do {
    isPrime=1;
    for (int i=3;i*i<n;i+=2) {
      if (n%i==0) isPrime=0;
    }
    if (isPrime) return n;
    n+=2;
  } while (1);
}
unsigned long count=0;
double stdSum=0.0,deltaSum=0.0;
void printStats(int i) {
  double mean=(samples[i]+0.0)/primes[i];
  double sum=0.0;
  unsigned min=count-1,max=0;
  for (int j=0;j<primes[i];j++) {
    unsigned value=counts[i][j];
    if (value>max) max=value;
    if (value<min) min=value;
    double diff=value-mean;
    sum+=diff*diff;
  }
  double std=sqrt(sum/primes[i]);
  stdSum+=std*100.0/mean;
  deltaSum+=(max-min)*100/mean;
  printf("%d std percent=%lf delta=%lf\n",primes[i],std*100.0/mean,(max-min)*100/mean);
}
int main(){
  unsigned int seed = 12345;
  unsigned int multiplier = 1664525;
  unsigned int increment = 1013904223;
  unsigned int n = seed;
  long printIt = 20;
  int repeat = 10000;
  int stillSampling;
  //for (int *p=primes,f=nextPrime(10);p<primes+NPrimes;f=nextPrime(f*1.75),++p) {
  //for (int *p=primes,f=4;p<primes+NPrimes;f=f+f,++p) {
  //for (int *p=primes,f=10;p<primes+NPrimes;f=f+f/3,++p) {
  //for (int *p=primes,f=60;p<primes+NPrimes;f=f+1,++p) {
  for (int *p=primes,f=nextPrime(60);p<primes+NPrimes;f=nextPrime(f+2),++p) {
    if (f>MaxBuckets) break;
    *p=f;
    ++nPrimes;
  }
  do {
    n = n*multiplier+increment;
    //if (--printIt>=0) printf("%u %u %u\n",n,n%11,n/1311%11);
    stillSampling = 0;
    for (int i=0;i<nPrimes;++i) {
      if (samples[i]<primes[i]*repeat) {
        ++samples[i];
        int t=n/201%primes[i];
        counts[i][t]++;
        stillSampling=1;
      }}
    count+=1;
  } while (stillSampling); //--repeat>0); //n != seed);
  printf("n=%u count=%lu multiplier=%u increment=%u\n",n,count,multiplier,increment);
  for (int i=0;i<nPrimes;++i)
    printStats(i);
  printf("average std percent=%lf delta=%lf\n",stdSum/nPrimes,deltaSum/nPrimes);

}
