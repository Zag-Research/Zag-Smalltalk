Benchmarking encoding: .onlyInt
info: Config:
  compile_date   = 2025-11-21T13:03:13+0000
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = apple_m1 (.aarch64)
  objectEncoding = .onlyInt
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  198ms   198ms   0.00ms   0.0%
  NativeF  305ms   305ms   0.00ms   0.0%
IntegerBr 3241ms  3241ms   0.49ms   0.0%
```

Benchmarking encoding: .onlyFloat
info: Config:
  compile_date   = 2025-11-21T13:03:49+0000
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = apple_m1 (.aarch64)
  objectEncoding = .onlyFloat
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  198ms   198ms   0.00ms   0.0%
  NativeF  305ms   305ms   0.00ms   0.0%
    Float 2646ms  2645ms   0.80ms   0.0%
```
Benchmarking encoding: .taggedInt
info: Config:
  compile_date   = 2025-11-21T13:04:19+0000
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = apple_m1 (.aarch64)
  objectEncoding = .taggedInt
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  199ms   198ms   0.40ms   0.2%
  NativeF  305ms   305ms   0.00ms   0.0%
IntegerBr 3374ms  3373ms   0.40ms   0.0%
    Float 4754ms  4754ms   0.40ms   0.0%
```
Benchmarking encoding: .nan
info: Config:
  compile_date   = 2025-11-21T13:05:33+0000
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = apple_m1 (.aarch64)
  objectEncoding = .nan
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  198ms   198ms   0.00ms   0.0%
  NativeF  305ms   305ms   0.00ms   0.0%
IntegerBr 3732ms  3735ms   5.85ms   0.2%
    Float 3136ms  3136ms   0.49ms   0.0%
```
Benchmarking encoding: .zag
info: Config:
  compile_date   = 2025-11-21T13:06:38+0000
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = apple_m1 (.aarch64)
  objectEncoding = .zag
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  199ms   199ms   0.00ms   0.0%
  NativeF  305ms   305ms   0.00ms   0.0%
IntegerBr 3634ms  3634ms   0.49ms   0.0%
    Float 3302ms  3302ms   0.00ms   0.0%
```
Benchmarking encoding: .zagAlt
info: Config:
  compile_date   = 2025-11-21T13:07:43+0000
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = apple_m1 (.aarch64)
  objectEncoding = .zagAlt
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  198ms   198ms   0.00ms   0.0%
  NativeF  305ms   305ms   0.00ms   0.0%
IntegerBr 3792ms  3789ms   4.53ms   0.1%
    Float 4500ms  4500ms   0.00ms   0.0%
```
Benchmarking encoding: .spur
info: Config:
  compile_date   = 2025-11-21T13:08:59+0000
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = apple_m1 (.aarch64)
  objectEncoding = .spur
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  198ms   198ms   0.00ms   0.0%
  NativeF  305ms   305ms   0.00ms   0.0%
IntegerBr 3375ms  3375ms   0.40ms   0.0%
    Float 4275ms  4275ms   0.98ms   0.0%
```
Benchmarking encoding: .ptr
info: Config:
  compile_date   = 2025-11-21T13:10:10+0000
  git_version    = 2025-11-21T08:01:47-05:00-1b547088
  cpu            = apple_m1 (.aarch64)
  objectEncoding = .ptr
  max_classes    = 255
  native_endian  = .little
  stack_size     = 511w
  nursery_size   = 3830w
  process_total_size = 8192w

```
for '40 fibonacci'
          Median   Mean   StdDev  SD/Mean (5 runs, 2 warmups)
   Native  198ms   198ms   0.00ms   0.0%
  NativeF  305ms   305ms   0.00ms   0.0%
IntegerBr 5348ms  5348ms   0.80ms   0.0%
    Float 5006ms  5006ms   0.40ms   0.0%
```
