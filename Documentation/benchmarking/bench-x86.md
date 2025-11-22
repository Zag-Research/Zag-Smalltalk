Benchmarking encoding: .onlyInt
info: Config:
  compile_date   = 2025-11-21T08:05:46-0500
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = skylake (.x86_64)
  objectEncoding = .onlyInt
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  164ms   163ms   0.49ms   0.3%
  NativeF  242ms   242ms   0.00ms   0.0%
IntegerBr 4951ms  4954ms  10.09ms   0.2%
```
Benchmarking encoding: .onlyFloat
info: Config:
  compile_date   = 2025-11-21T08:06:38-0500
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = skylake (.x86_64)
  objectEncoding = .onlyFloat
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  144ms   144ms   0.00ms   0.0%
  NativeF  242ms   242ms   0.00ms   0.0%
    Float 4356ms  4355ms   0.80ms   0.0%
```
Benchmarking encoding: .taggedInt
info: Config:
  compile_date   = 2025-11-21T08:07:24-0500
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = skylake (.x86_64)
  objectEncoding = .taggedInt
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  144ms   144ms   0.49ms   0.3%
  NativeF  242ms   242ms   0.40ms   0.2%
IntegerBr 5374ms  5373ms   1.47ms   0.0%
    Float 7413ms  7412ms   2.65ms   0.0%
```
Benchmarking encoding: .nan
info: Config:
  compile_date   = 2025-11-21T08:09:19-0500
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = skylake (.x86_64)
  objectEncoding = .nan
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  169ms   168ms   0.49ms   0.3%
  NativeF  255ms   255ms   0.00ms   0.0%
IntegerBr 5496ms  5495ms   4.18ms   0.1%
    Float 4747ms  4755ms  18.41ms   0.4%
```
Benchmarking encoding: .zag
info: Config:
  compile_date   = 2025-11-21T08:10:54-0500
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = skylake (.x86_64)
  objectEncoding = .zag
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  131ms   131ms   0.00ms   0.0%
  NativeF  295ms   295ms   0.00ms   0.0%
IntegerBr 5280ms  5280ms   2.06ms   0.0%
    Float 5012ms  5012ms   1.02ms   0.0%
```
Benchmarking encoding: .zagAlt
info: Config:
  compile_date   = 2025-11-21T08:12:29-0500
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = skylake (.x86_64)
  objectEncoding = .zagAlt
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  164ms   163ms   0.49ms   0.3%
  NativeF  249ms   248ms   0.40ms   0.2%
IntegerBr 5656ms  5655ms   0.75ms   0.0%
    Float 6564ms  6565ms   2.10ms   0.0%
```
Benchmarking encoding: .spur
info: Config:
  compile_date   = 2025-11-21T08:14:19-0500
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = skylake (.x86_64)
  objectEncoding = .spur
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  163ms   163ms   0.40ms   0.2%
  NativeF  248ms   248ms   0.49ms   0.2%
IntegerBr 5165ms  5165ms   3.43ms   0.1%
    Float 6526ms  6527ms   4.12ms   0.1%
```
Benchmarking encoding: .ptr
install
+- install fib
   +- compile exe fib ReleaseFast native failure
error: the following command terminated unexpectedly:
/usr/local/share/zig-x86_64-linux-0.15.1/zig build-exe -OReleaseFast --dep zag -Mroot=/home/dmason/git/Zag-Smalltalk/zig/experiments/fib.zig --dep options -Mzag=/home/dmason/git/Zag-Smalltalk/zig/zag/zag.zig -Moptions=.zig-cache/c/01033b8c39611ed5bf64da3afd522c6c/options.zig --cache-dir .zig-cache --global-cache-dir /home/dmason/.cache/zig --name fib --zig-lib-dir /usr/local/share/zig-x86_64-linux-0.15.1/lib/ --listen=-

Build Summary: 1/4 steps succeeded; 1 failed
install transitive failure
+- install fib transitive failure
   +- compile exe fib ReleaseFast native failure

error: the following build command failed with exit code 1:
.zig-cache/o/66405a23332f145402b87abfebe1769b/build /usr/local/share/zig-x86_64-linux-0.15.1/zig /usr/local/share/zig-x86_64-linux-0.15.1/lib /home/dmason/git/Zag-Smalltalk/zig .zig-cache /home/dmason/.cache/zig --seed 0xace96952 -Z355d53f1f487e551 -Dencoding=ptr -Doptimize=ReleaseFast
