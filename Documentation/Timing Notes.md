### AARCH64 Apple M2 Pro
#### floatZag
zag time: 3253175875ns 3806338333ns 2560578792ns
Spur time: 4466756208ns 5596710917ns 3774775750ns
Zag is 1.37x 1.47x 1.47x faster
#### Native
   Native  174ms   173ms   0.83ms   0.5%
  NativeF  251ms   250ms   0.75ms   0.3%

#### zag
tag **fastest**
	IntegerBr 2469ms  2467ms   5.10ms   0.2%
    Float 3118ms  3120ms  10.13ms   0.3%
    IntegerBr 2512ms  2508ms   8.32ms   0.3%
    Float 3124ms  3123ms   5.99ms   0.2%
table
	IntegerBr 2480ms  2478ms  11.53ms   0.5%
    Float 3112ms  3114ms   9.68ms   0.3%
    IntegerBr 2471ms  2470ms   1.74ms   0.1%
    Float 3131ms  3136ms   8.57ms   0.3%
firstFloat
	IntegerBr 2498ms  2513ms  35.62ms   1.4%
    Float 3112ms  3115ms  11.63ms   0.4%
andTagbits
	IntegerBr 2525ms  2522ms  11.31ms   0.4%
    Float 3202ms  3209ms  17.60ms   0.5%
bigSwitch
	IntegerBr 2557ms  2559ms  17.09ms   0.7%
    Float 3358ms  3358ms   1.96ms   0.1%
rotateTagbits
	IntegerBr 2583ms  2577ms  18.66ms   0.7%
    Float 3188ms  3188ms   5.42ms   0.2%
#### zagAlt
    IntegerBr 2456ms  2455ms   2.04ms   0.1%
    Float 3879ms  3883ms  11.03ms   0.3%
#### nan
nanCompare **fastest**
    IntegerBr 2313ms  2309ms  11.02ms   0.5%
    Float 2274ms  2274ms   0.98ms   0.0%
fullCompare
    IntegerBr 2299ms  2310ms  15.13ms   0.7%
    Float 2278ms  2280ms   5.64ms   0.2%
tagCompare
	IntegerBr 2361ms  2361ms   1.94ms   0.1%
    Float 2327ms  2334ms  18.30ms   0.8%
bigSwitch
	IntegerBr 2417ms  2430ms  17.45ms   0.7%
    Float 2364ms  2384ms  39.19ms   1.6%
nanSwitch
    IntegerBr 2336ms  2337ms   2.83ms   0.1%
    Float 2292ms  2289ms  10.87ms   0.5%

### x86-64  Intel i7-9750H CPU @ 2.60GHz
#### floatZag
zag time: 14590432471ns 28238676052ns 13132381948ns
Spur time: 14412543207ns 29414343484ns 4638949801ns
Zag is 0.99x 1.04x 0.35x faster
#### Native
   Native  159ms   158ms   0.49ms   0.3%
  NativeF  267ms   269ms   3.52ms   1.3%
#### zag
IntegerBr 5245ms  5254ms  27.35ms   0.5%
    Float 6190ms  6187ms  10.09ms   0.2%
table **fastest**
	IntegerBr 4987ms  4983ms  17.87ms   0.4%
	    Float 5694ms  5694ms  10.67ms   0.2%
firstFloat
	IntegerBr 5003ms  5001ms  12.74ms   0.3%
	    Float 5565ms  5649ms 135.04ms   2.4%
andTagbits
	IntegerBr 5037ms  5038ms  14.04ms   0.3%
	    Float 5723ms  5718ms   7.00ms   0.1%
bigSwitch
	IntegerBr 5089ms  5081ms  14.97ms   0.3%
	    Float 5741ms  5745ms  10.59ms   0.2%
tag
	IntegerBr 6080ms  6100ms 336.27ms   5.5%
	   Float 6555ms  6567ms  50.01ms   0.8%
rotateTagbits
	IntegerBr 6236ms  6150ms 133.37ms   2.2%
	    Float 6118ms  6216ms 199.56ms   3.2%

#### zagAlt
IntegerBr 4612ms  4596ms  38.37ms   0.8%
    Float 6297ms  6312ms  24.00ms   0.4%
#### nan
IntegerBr 4444ms  4458ms  26.68ms   0.6%
    Float 4246ms  4248ms   7.09ms   0.2%
tagCompare **fastest**
	IntegerBr 4735ms  4738ms  20.95ms   0.4%
	    Float 4671ms  4669ms  28.62ms   0.6%
bigSwitch
	IntegerBr 4814ms  4814ms  11.34ms   0.2%
	    Float 4578ms  4576ms  17.32ms   0.4%
 fullCompare
    IntegerBr 4761ms  4836ms 147.11ms   3.0%
	    Float 4709ms  4710ms  12.72ms   0.3%
   
