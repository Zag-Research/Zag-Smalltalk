# Float Encoding Rankings

This note records the local ReleaseFast microbenchmark rankings for immediate
float encodings in `zig/zag/encoding/floatEncoding.zig`.

The benchmark reports three timings:

- `valid encode`: encoding values that fit the candidate.
- `invalid encode`: attempting values outside the candidate's encodable range.
- `decode`: decoding already-encoded immediate floats.

All numbers below are nanoseconds per operation from local x86_64 runs. Treat
the absolute values as machine-specific; use them to compare candidates within a
single run. aarch64 still needs a separate run before treating the ranking as
cross-architecture.

## Combined Ranking

`Fst2P1OrZero` is the current best balanced same-layout candidate. It keeps the
same tag use and `97.21%` coverage as `Fst2(4)`/`Fst2FastApi`, but uses a
zero-sentinel `encodeOrZero` fast path.

The baseline `Fst2(4)` run was `0.554ns` valid encode, `0.548ns` invalid encode,
`0.565ns` decode, with `97.21%` coverage. When a candidate appeared in multiple
runs, the later follow-up measurement is shown. The ranking is by practical VM
usefulness for the current small-tag-budget goal, not by valid-encode time
alone.

| Rank | Candidate | Valid encode | Invalid encode | Decode | Coverage | Verdict |
| ---: | --- | ---: | ---: | ---: | ---: | --- |
| 1 | `Fst2P1OrZero` | 0.430ns | 0.543ns | 0.539ns | 97.21% | Chosen; best balanced same-layout encode path. |
| 2 | `Fst2P1XorZero` | 0.419ns | 0.787ns | 0.506ns | 97.21% | Fastest valid encode, but invalid path is too slow for production. |
| 3 | `Fst2OrZero` | 0.540ns | 0.542ns | 0.505ns | 97.21% | Good sentinel API, but valid encode does not improve enough. |
| 4 | `Fst2FastApi` | 0.537ns | 0.791ns | 0.499ns | 97.21% | Previous best API split; decode remains strong. |
| 5 | `Fst2KnownDecode` | 0.546ns | 0.547ns | 0.535ns | 97.21% | Best when caller already knows it has a float; not safe for generic `nativeF`. |
| 6 | `Fst2Precheck` | 0.544ns | 0.546ns | 0.526ns | 97.21% | Avoids the bad invalid path, but valid encode trails. |
| 7 | `Fst2ExpZero` | 0.542ns | 0.540ns | 0.537ns | 97.21% | Balanced but not faster than `Fst2P1OrZero`. |
| 8 | `Fst2ExpRFirst` | 0.544ns | 0.540ns | 0.539ns | 97.21% | Balanced, but no win over the sentinel P1 variant. |
| 9 | `Fst2GenXor` | 0.541ns | 0.547ns | 0.580ns | 97.21% | Valid/invalid okay, decode worse. |
| 10 | `Fst2Strict` | 0.537ns | 0.787ns | 0.539ns | 97.21% | Good valid/decode, bad invalid path. |
| 11 | `Fst2P1Xor` | 0.551ns | 0.544ns | 0.499ns | 97.21% | Decode good, no encode win. |
| 12 | `Fst2P1AddZero` | 0.552ns | 0.543ns | 0.540ns | 97.21% | Sentinel form did not help this arithmetic variant. |
| 13 | `Fst2P1Add` | 0.552ns | 0.544ns | 0.541ns | 97.21% | No win over baseline. |
| 14 | `Fst2P1Or` | 0.544ns | 0.792ns | 0.506ns | 97.21% | Needs zero-sentinel form to win. |
| 15 | `Fst2PreLt` | 0.552ns | 0.545ns | 0.540ns | 97.21% | Slower comparison form. |
| 16 | `Fst2XorMask` | 0.546ns | 0.544ns | 0.544ns | 97.21% | Cleaner predicate, no practical win. |
| 17 | `Fst2BitPair` | 0.557ns | 0.550ns | 0.549ns | 97.21% | Splitting the predicate is slower. |
| 18 | `Fst2High5Zero` | 0.560ns | 0.542ns | 0.538ns | 97.21% | High-bit precheck did not beat low-bit P1 check. |
| 19 | `Fst2High5BZero` | 0.576ns | 0.543ns | 0.506ns | 97.21% | Good decode, encode too slow. |
| 20 | `Fst2High5P1` | 0.569ns | 0.786ns | 0.500ns | 97.21% | High-bit path regresses invalid encode. |
| 21 | `Fst2High5U5` | 0.571ns | 0.806ns | 0.502ns | 97.21% | Type-narrowed high-bit path did not help. |
| 22 | `Fst2NoHint` | 0.595ns | 0.605ns | 0.595ns | 97.21% | Branch hints help; removing them loses. |
| 23 | `Zag6` | 0.495ns | 0.549ns | 0.571ns | 100.00% | Strong if six tags are acceptable. |
| 24 | `Fst4` | 0.557ns | 0.560ns | 0.554ns | 99.11% | Four tags, more coverage, but no same-budget win. |
| 25 | `Zag4` | 0.589ns | 0.562ns | 0.545ns | 99.11% | Four-tag layout; slower valid encode. |
| 26 | `Fst2Finite` | 1.253ns | 0.787ns | 1.413ns | 98.36% | Coverage improves; speed loses badly. |
| 27 | `Map3Exp16` | 1.866ns | 1.069ns | 2.202ns | 99.09% | Coverage is good, mapping cost kills it. |
| 28 | `Map4Best` | 1.910ns | 1.216ns | 2.660ns | 99.81% | High coverage, table mapping cost too high. |
| 29 | `Map6Best` | 2.264ns | 1.185ns | 2.459ns | 100.00% | Full coverage, much slower than hand-coded layouts. |
| 30 | `Map2Finite` | 2.170ns | 1.154ns | 1.939ns | 98.36% | Worse than hand-coded finite mapping. |
| 31 | `Map2Current` | 2.121ns | 1.409ns | 1.936ns | 97.21% | Same coverage as current, much slower. |

## Notes

- The benchmark harness now recognizes `encodeOrZero`, `encodeOptional`, and
  `decodeKnown` APIs so candidate shapes can be compared without forcing every
  candidate through the error-union API.
- `Fst2P1OrZero` is equivalent to the `Fst2(4)` layout, but rewrites the encode
  check as `p = rotl(bits, 5) + 1`; valid values satisfy `(p & 6) == 0`, and
  the encoded value is `p | 4`.
- `Fst2P1XorZero` is slightly faster on valid inputs in one run, but loses on
  invalid inputs, so it is not the current production choice.
- `decodeKnown` is only safe when the caller has already established that the
  object is an immediate float. Generic object decoding still needs a tag check.
