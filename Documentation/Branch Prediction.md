
Branch prediction is based on the current PC, so a threaded word for send will have jumps to the various locations from a fixed location, so branch prediction becomes useless. Localizing branches is probably the biggest win from Copy&Patch JIT. Could do a very simple experiment on C&P by when it is about to do the jump, instead jump to a fixed point that does the indirect jump - whatever slowdown we see would be the failure of branch prediction.


[Experiments with x86 & M1](https://blog.cloudflare.com/branch-predictor/)

[StackOverflow discussion](https://stackoverflow.com/questions/11227809/why-is-processing-a-sorted-array-faster-than-processing-an-unsorted-array) has a lot of good explanation.

The `branchPrediction` program runs similar tests. All except the first two are anding a constant with the index (going from 0 to 0x40000009 - a prime number just over a billion). Both the then and else do the same work (an `^=` on different variables) that the optimizer can't optimize away. All except the first 4 and last 2 are symmetric (same number of true and false branches), so any variation is because of the branch prediction algorithm. Each sample is the mean of 5 runs with 1 warmup run ± the standard deviation as a percentage of the mean. The third column is the time of just the branches (with the loop overhead and work removed) and shows the actual range of mis-prediction costs.

On an Apple M2 Pro [MacBook](https://reflexive.space/apple-m2-bp/) [and](https://www.realworldtech.com/forum/?threadid=214921&curpostid=215099) I get:
```
True        : 116ms±0.6%
False       : 117ms±2.1%
T_repeated  : 207ms±2.0%  91ms
F_repeated  : 203ms±0.9%  87ms
TF          : 206ms±1.0%  90ms
T2F2        : 220ms±0.8% 104ms
T4F4        : 238ms±3.1% 122ms
T8F8        : 241ms±1.4% 125ms
T16F16      : 246ms±2.5% 139ms
T32F32      : 239ms±0.9% 123ms
T64F64      : 243ms±2.0% 127ms
ThalfFhalf  : 241ms±0.9% 125ms
TFFF        : 236ms±1.9% 120ms
TFTFFFFF    : 244ms±1.4% 128ms
```

On an Apple Intel i7-2.6GHz MacBook Pro I get:
```
True        : 46ms±2.6%
False       : 46ms±3.3%
T_repeated  : 169ms±1.0%
F_repeated  : 166ms±1.2%
TF          : 119ms±1.6%
T2F2        : 118ms±2.7%
TFFF        : 115ms±2.5%
T4F4        : 116ms±2.7%
TFTFFFFF    : 140ms±1.1%
T8F8        : 141ms±2.1%
T16F16      : 146ms±0.8%
T32F32      : 148ms±1.3%
T64F64      : 149ms±1.8%
```
