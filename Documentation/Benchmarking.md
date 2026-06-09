[Shootout benchmarks](https://en.wikipedia.org/wiki/The_Computer_Language_Benchmarks_Game#Benchmark_programs): [Interesting analysis on experessivity vs. performance](http://blog.gmarceau.qc.ca/2009/05/speed-size-and-dependability-of.html)
- nbody [benchmark game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/nbody.html) [AWFY](https://github.com/smarr/are-we-fast-yet/tree/master/benchmarks/Smalltalk/NBody)
- threadring 
- binarytrees

[Scheme R7RS Benchmarks](github.com/ecraven/r7rs-benchmarks) 60 Scheme benchmarks

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
## Data Sources
[Public BI benchmark](https://github.com/cwida/public_bi_benchmark) has some very large user-donated datasets
## Benchmarking Encodings
There are currently two runs of the `bench` script on an [Apple M1](benchmarking/bench-m1) and an [X86-64](benchmarking/bench-x86).

To extract and compare just the results, in the directory you can do:
```shell
grep 'ctEnco\|IntegerB\|Float ' bench-m1
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

## Minimizing interference on Linux
For benchmarking **CPU compute**, achieving repeatability is all about eliminating **non-deterministic** hardware and software behavior. In addition to minimizing background processes, you should focus on **CPU isolation** and **throttling prevention**. 

1. CPU Isolation (Pinning your benchmark)

The most effective way to ensure repeatability is to reserve specific CPU cores strictly for your benchmark, preventing the Linux kernel from scheduling any other tasks on them. 

- **Isolate Cores via GRUB**: You can "hide" cores from the OS by editing `/etc/default/grub`.
    
    bash
    
    ```
    # Example: Isolating cores 1, 2, and 3 on a 4-core system
    GRUB_CMDLINE_LINUX_DEFAULT="... isolcpus=1-3"
    ```
    
    Use code with caution.
    
    After updating (`sudo update-grub`) and rebooting, the OS will only use Core 0 for general tasks.
- **Run on Isolated Cores**: Use `taskset` to force your benchmark to run only on those reserved cores.
    
    bash
    
    ```
    taskset -c 1-3 ./your_benchmark
    ```
    
    Use code with caution.
    
     

2. Force Consistent Frequencies

Modern CPUs fluctuate their clock speeds rapidly based on load and temperature. For benchmarks, you want a **fixed** frequency. 

- **Disable Turbo Boost**: Turbo Boost is great for bursts but bad for consistency because it depends on thermal headroom.
    - **Intel**: `echo 1 | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo`.
    - **AMD**: `echo 0 | sudo tee /sys/devices/system/cpu/cpufreq/boost`.
- **Performance Governor**: Ensure the CPU is locked at its highest base frequency.
    
    bash
    
    ```
    echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
    ```

3. Disable Hyper-Threading (SMT) 

If your benchmark is highly compute-intensive, two logical threads sharing one physical core can cause "resource contention," leading to inconsistent scores. 

- **Action**: Disable Hyper-Threading in your **BIOS/UEFI** before testing. This ensures each process has exclusive access to the physical core's execution units. 

4. Advanced "Tickless" Kernel Settings 

Even on an isolated core, the kernel usually "wakes up" the CPU 100–1000 times a second to check for tasks (the "timer tick"). You can minimize this "jitter" by telling the kernel to go "tickless" on your isolated cores: 

- Add `nohz_full=1-3` to your GRUB command line (using the same core numbers as `isolcpus`). 

5. Disable Address Space Layout Randomization (ASLR) 

ASLR moves data around in RAM for security, which can cause slight variations in cache performance and memory access times. 

- **Temporary disable**: `echo 0 | sudo tee /proc/sys/kernel/randomize_va_space`. 

Summary Checklist for a "Pure" Compute Environment

1. **Reboot** into a `multi-user.target` (non-GUI) environment.
2. **Disable Turbo Boost** and set the **Performance Governor**.
3. **Disable ASLR** to stabilize memory access patterns.
4. **Pin your process** to specific cores using `taskset`.
5. **Wait 5 minutes** after boot for all "startup" background noise to settle before starting the first run.

## Victor Stinner blog
[perf module](https://vstinner.github.io/perf-visualize-system-noise-with-cpu-isolation.html)
My journey to stable benchmark, [part 1 (system)](https://vstinner.github.io/journey-to-stable-benchmark-system.html) [part 2 (deadcode)](https://vstinner.github.io/journey-to-stable-benchmark-deadcode.html)  [part 3 (average)](https://vstinner.github.io/journey-to-stable-benchmark-average.html)
Intel CPUs: [P-state, C-state, Turbo Boost, CPU frequency, etc.](https://vstinner.github.io/intel-cpus.html) [Turbo Boost, temperature, frequency and Pstate C0 bug](https://vstinner.github.io/intel-cpus-part2.html)
