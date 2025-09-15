AARCH64 floatZag
zag time: 3317687084ns 3796121833ns 2547608000ns
Spur time: 4524146417ns 6129791458ns 3589476542ns
Zag is 1.36x 1.61x 1.41x faster

AARCH64 with zag
   Native  174ms   173ms   0.83ms   0.5%
  NativeF  251ms   250ms   0.75ms   0.3%
IntegerBr 2519ms  2513ms   8.98ms   0.4%
    Float 3134ms  3136ms  15.05ms   0.5%

AARCH64 with zagAlt
   Native  175ms   175ms   1.60ms   0.9%
  NativeF  251ms   252ms   6.38ms   2.5%
IntegerBr 2512ms  2508ms   7.43ms   0.3%
    Float 3791ms  3788ms  18.72ms   0.5%

AARCH64 with nan
   Native  171ms   171ms   0.54ms   0.3%
  NativeF  248ms   248ms   1.60ms   0.6%
IntegerBr 2554ms  2557ms   8.73ms   0.3%
    Float 2492ms  2500ms  18.45ms   0.7%