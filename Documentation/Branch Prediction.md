
Branch prediction is based on the current PC, so a threaded word for send will have jumps to the various locations from a fixed location, so branch prediction becomes useless. Localizing branches is probably the biggest win from Copy&Patch JIT. Could do a very simple experiment on C&P by when it is about to do the jump, instead jump to a fixed point that does the indirect jump - whatever slowdown we see would be the failure of branch prediction.


[Experiments with x86 & M1](https://blog.cloudflare.com/branch-predictor/)

[StackOverflow discussion](https://stackoverflow.com/questions/11227809/why-is-processing-a-sorted-array-faster-than-processing-an-unsorted-array) has a lot of good explanation.

The `branchPrediction` program runs similar tests. On an Apple M2 Pro MacBook I get:
```
LM6PC7X2M6
True        : 117ms±0.8%
False       : 117ms±1.1%
T_repeated  : 205ms±1.6%
F_repeated  : 208ms±0.6%
TF          : 206ms±1.0%
T2F2        : 219ms±0.6%
TFFF        : 239ms±1.8%
T4F4        : 234ms±0.6%
TFTFFFFF    : 243ms±3.3%
T8F8        : 245ms±1.8%
T16F16      : 243ms±2.1%
T32F32      : 245ms±1.1%
T64F64      : 242ms±1.6%
```
