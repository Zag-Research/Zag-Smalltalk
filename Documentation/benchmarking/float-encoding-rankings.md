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

## Current Recommendation

`Fst2P1OrZero` is the current best balanced candidate from the follow-up pass.
It keeps the same tag use and `97.21%` coverage as `Fst2(4)`/`Fst2FastApi`, but
uses a zero-sentinel `encodeOrZero` fast path:

| Candidate | Valid encode | Invalid encode | Decode | Coverage | Verdict |
| --- | ---: | ---: | ---: | ---: | --- |
| `Fst2P1OrZero` | 0.430ns | 0.543ns | 0.539ns | 97.21% | Best balanced same-layout result. |
| `Fst2P1XorZero` | 0.419ns | 0.787ns | 0.506ns | 97.21% | Fastest valid encode, but invalid path regresses. |
| `Fst2FastApi` | 0.537ns | 0.791ns | 0.499ns | 97.21% | Previous best API split; still best known decode. |

## First Candidate Batch

Baseline `Fst2(4)` from this run:

| Candidate | Valid encode | Invalid encode | Decode | Coverage |
| --- | ---: | ---: | ---: | ---: |
| `Fst2(4)` | 0.554ns | 0.548ns | 0.565ns | 97.21% |

Ranking from the first candidate batch:

| Rank | Candidate | Valid encode | Invalid encode | Decode | Coverage | Verdict |
| ---: | --- | ---: | ---: | ---: | ---: | --- |
| 1 | `Fst2FastApi` | 0.533ns | 0.547ns | 0.560ns | 97.21% | Best same-layout improvement in the first pass. |
| 2 | `Fst2KnownDecode` | 0.546ns | 0.547ns | 0.535ns | 97.21% | Best when caller already knows it has a float; not safe for generic `nativeF`. |
| 3 | `Zag6` | 0.562ns | 0.549ns | 0.565ns | 100.00% | Excellent if six tags are available; poor general VM tradeoff. |
| 4 | `Fst4` | 0.494ns | 0.796ns | 0.551ns | 99.11% | Good float-heavy option, but uses four tags and has a bad invalid path. |
| 5 | `Zag4` | 0.562ns | 0.552ns | 0.614ns | 99.11% | Higher coverage, but slower decode and a larger tag tradeoff. |
| 6 | `Fst2Strict` | 0.540ns | 0.788ns | 0.547ns | 97.21% | Faster valid/decode, worse invalid; not a clean win. |
| 7 | `Fst2Finite` | 1.253ns | 0.787ns | 1.413ns | 98.36% | Coverage improves; speed loses badly. |
| 8 | `Map3Exp16` | 1.866ns | 1.069ns | 2.202ns | 99.09% | Coverage is good, mapping cost kills it. |
| 9 | `Map4Best` | 2.149ns | 1.153ns | 2.169ns | 99.81% | Theoretical coverage win, practical loss. |
| 10 | `Map6Best` | 2.175ns | 1.053ns | 2.400ns | 100.00% | Full coverage but much slower than `Zag6`. |
| 11 | `Map2Finite` | 2.170ns | 1.154ns | 1.939ns | 98.36% | Worse than hand-coded finite mapping. |
| 12 | `Map2Current` | 2.121ns | 1.409ns | 1.936ns | 97.21% | Same coverage as current, much slower. |
| 13 | `Fst2NoHint` | 0.595ns | 0.605ns | 0.595ns | 97.21% | Branch hints help; removing them loses. |

## Follow-Up `Fst2FastApi` Improvement Pass

The second pass focused on preserving the two-tag `4,5` layout while improving
the production encode path. Ranking below is by production usefulness: same tag
budget and coverage first, then valid encode performance, with invalid/decode
used as tie breakers.

| Rank | Candidate | Valid encode | Invalid encode | Decode | Coverage | Verdict |
| ---: | --- | ---: | ---: | ---: | ---: | --- |
| 1 | `Fst2P1OrZero` | 0.430ns | 0.543ns | 0.539ns | 97.21% | Chosen; best balanced valid/invalid encode path. |
| 2 | `Fst2P1XorZero` | 0.419ns | 0.787ns | 0.506ns | 97.21% | Fastest valid encode, but invalid path is too slow for production. |
| 3 | `Fst2OrZero` | 0.540ns | 0.542ns | 0.505ns | 97.21% | Good sentinel API, but valid encode does not improve enough. |
| 4 | `Fst2ExpRFirst` | 0.544ns | 0.540ns | 0.539ns | 97.21% | Balanced, but no win over the sentinel P1 variant. |
| 5 | `Fst2FastApi` | 0.537ns | 0.791ns | 0.499ns | 97.21% | Previous best; decode remains strong. |
| 6 | `Fst2Precheck` | 0.544ns | 0.546ns | 0.526ns | 97.21% | Avoids the bad invalid path, but valid encode trails. |
| 7 | `Fst2ExpZero` | 0.542ns | 0.540ns | 0.537ns | 97.21% | Balanced but not faster than `Fst2P1OrZero`. |
| 8 | `Fst2GenXor` | 0.541ns | 0.547ns | 0.580ns | 97.21% | Valid/invalid okay, decode worse. |
| 9 | `Fst2Strict` | 0.537ns | 0.787ns | 0.539ns | 97.21% | Good valid/decode, bad invalid path. |
| 10 | `Fst2High5Zero` | 0.560ns | 0.542ns | 0.538ns | 97.21% | High-bit precheck did not beat low-bit P1 check. |
| 11 | `Fst2High5BZero` | 0.576ns | 0.543ns | 0.506ns | 97.21% | Good decode, encode too slow. |
| 12 | `Fst2P1Xor` | 0.551ns | 0.544ns | 0.499ns | 97.21% | Decode good, no encode win. |
| 13 | `Fst2P1Add` | 0.552ns | 0.544ns | 0.541ns | 97.21% | No win over baseline. |
| 14 | `Fst2P1AddZero` | 0.552ns | 0.543ns | 0.540ns | 97.21% | Sentinel form did not help this arithmetic variant. |
| 15 | `Fst2P1Or` | 0.544ns | 0.792ns | 0.506ns | 97.21% | Needs zero-sentinel form to win. |
| 16 | `Fst2PreLt` | 0.552ns | 0.545ns | 0.540ns | 97.21% | Slower comparison form. |
| 17 | `Fst2XorMask` | 0.546ns | 0.544ns | 0.544ns | 97.21% | Cleaner predicate, no practical win. |
| 18 | `Fst2BitPair` | 0.557ns | 0.550ns | 0.549ns | 97.21% | Splitting the predicate is slower. |
| 19 | `Fst2High5P1` | 0.569ns | 0.786ns | 0.500ns | 97.21% | High-bit path regresses invalid encode. |
| 20 | `Fst2High5U5` | 0.571ns | 0.806ns | 0.502ns | 97.21% | Type-narrowed high-bit path did not help. |

## Coverage-Oriented Alternatives

These can encode more floats, but they are not better for the current "run fast
with a small tag budget" goal.

| Candidate | Valid encode | Invalid encode | Decode | Coverage | Verdict |
| --- | ---: | ---: | ---: | ---: | --- |
| `Zag6` | 0.495ns | 0.549ns | 0.571ns | 100.00% | Strong if six tags are acceptable. |
| `Fst4` | 0.557ns | 0.560ns | 0.554ns | 99.11% | Four tags, more coverage, but no same-budget win. |
| `Zag4` | 0.589ns | 0.562ns | 0.545ns | 99.11% | Four-tag layout; slower valid encode. |
| `Map4Best` | 1.910ns | 1.216ns | 2.660ns | 99.81% | High coverage, table mapping cost too high. |
| `Map6Best` | 2.264ns | 1.185ns | 2.459ns | 100.00% | Full coverage, much slower than hand-coded layouts. |

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
