One of the contributions of Zag is analysis of various object encodings

[NaN](NaN.md) was the original encoding considered for Zag Smalltalk. It has good Float performance since it encodes all floating point values as immediate values, has mediocre integer performance, and can encode a fairly wide range of immediate values.

[ZagOrig](ZagOrig.md) was the next iteration. It has good Float performance and encodes about 3/4 of floating point values as immediates, somewhat better but still mediocre integer performance, and can encode a very broad range of immediate values.

[Zag](Zag.md) is the current encoding used by Zag Smalltalk. It has good Float performance and encodes the most common 1/4 of floating point values (that covers 97% of actual usage) as immediates, has exceptional integer performance, and can encode the broadest range of immediate values, including native pointers.

[Spur](Spur.md) is the encoding used by the OpenSmalltalkVM. It has mediocre Float performance and encodes the most common floating point values as immediates (reducing the exponent by 3 bits), has exceptional integer performance, and of the remaining classes encodes only `Character` as immediate values.

Ptr

TaggedInt