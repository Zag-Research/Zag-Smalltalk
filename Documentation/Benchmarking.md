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