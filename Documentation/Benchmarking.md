[Shootout benchmarks](https://en.wikipedia.org/wiki/The_Computer_Language_Benchmarks_Game#Benchmark_programs): [Interesting analysis on experessivity vs. performance](http://blog.gmarceau.qc.ca/2009/05/speed-size-and-dependability-of.html)
- nbody [benchmark game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/nbody.html) [AWFY](https://github.com/smarr/are-we-fast-yet/tree/master/benchmarks/Smalltalk/NBody)
- threadring 
- binarytrees

Integer benchmarks (fibonacci, integer benchmarks),

 [Clément Béra](https://clementbera.wordpress.com/ "Clément Béra") [on Cog optimizations](https://clementbera.wordpress.com/2014/08/12/arithmetic-inlined-and-special-selectors/) including how to turn them off (for benchmarking comparisons)

 [This](https://github.com/hashrocket/websocket-shootout/issues/44) bug/comment thread has nothing to do with our benchmarking, but still has a lot of good insights about benchmarking in general.

In [Resilient Smalltalk] there is a micro-benchmark testing recursion and non-local returns:
```smalltalk
Element = Object (
| next |
length = (
    | n |
    n := 0.
    self do: [ :e | n := n + 1. e ifLast: [ ˆn ]. ].
)
do: [block] = (
    block value: self.
    next do: block.
)
ifLast: [block] = (
    next isNil ifTrue: [ block value ].
)
)
```

## Benchmarking Encodings
There are currently two runs of the `bench` script on an [Apple M1](benchmarking/bench-m1) and an [X86-64](benchmarking/bench-x86).

To extract and compare just the results, in the directory you can do:
```shell
grep 'ctEnco\|IntegerB' bench-m1;grep 'ctEnco\|Float ' bench-m1
```
The current values are (1st number is median, 2nd mean, 3rd&4th variance):
```
  objectEncoding = .onlyInt
IntegerBr 3241ms  3241ms   0.49ms   0.0%
  objectEncoding = .onlyFloat
    Float 2646ms  2645ms   0.80ms   0.0%
  objectEncoding = .taggedInt
IntegerBr 3374ms  3373ms   0.40ms   0.0%
    Float 4754ms  4754ms   0.40ms   0.0%
  objectEncoding = .nan
IntegerBr 3732ms  3735ms   5.85ms   0.2%
    Float 3136ms  3136ms   0.49ms   0.0%
  objectEncoding = .zag
IntegerBr 3634ms  3634ms   0.49ms   0.0%
    Float 3302ms  3302ms   0.00ms   0.0%
  objectEncoding = .zagAlt
IntegerBr 3792ms  3789ms   4.53ms   0.1%
    Float 4500ms  4500ms   0.00ms   0.0%
  objectEncoding = .spur
IntegerBr 3375ms  3375ms   0.40ms   0.0%
    Float 4275ms  4275ms   0.98ms   0.0%
  objectEncoding = .ptr
IntegerBr 5348ms  5348ms   0.80ms   0.0%
    Float 5006ms  5006ms   0.40ms   0.0%
```
As these are run on computers in single-user mode, and have essentially 0 variance, these can be taken as accurate benchmark values. There are several values that should be almost identical (`spur` and `zagAlt`, float `taggedInt` and `ptr`) but are several percent different. While much of the code should be the same between versions (`zagAlt` and `spur` should be almost identical for these tests). But because the code was written at different times, and things were added, differences crept in. To compare two implementations, try the following command:
```shell
diff spur.zig zagAlt.zig |less
```
