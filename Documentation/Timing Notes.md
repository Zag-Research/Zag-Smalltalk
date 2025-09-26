### AARCH64 Apple M2 Pro
#### floatZag
zag time: 3253175875ns 3806338333ns 2560578792ns
Spur time: 4466756208ns 5596710917ns 3774775750ns
Zag is 1.37x 1.47x 1.47x faster
#### Native
   Native  174ms   173ms   0.83ms   0.5%
  NativeF  251ms   250ms   0.75ms   0.3%

#### zag
IntegerBr 2519ms  2513ms   8.98ms   0.4%
    Float 3134ms  3136ms  15.05ms   0.5%

#### zagAlt
IntegerBr 2512ms  2508ms   7.43ms   0.3%
    Float 3791ms  3788ms  18.72ms   0.5%

#### nan
IntegerBr 2554ms  2557ms   8.73ms   0.3%
    Float 2492ms  2500ms  18.45ms   0.7%

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
#### zagAlt
IntegerBr 4612ms  4596ms  38.37ms   0.8%
    Float 6297ms  6312ms  24.00ms   0.4%
#### nan
IntegerBr 4444ms  4458ms  26.68ms   0.6%
    Float 4246ms  4248ms   7.09ms   0.2%