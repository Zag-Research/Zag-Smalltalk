Extension { #name : 'SmallInteger' }

{ #category : '*Zag-Exporter' }
SmallInteger >> bitHash [

 "Source: https://github.com/skeeto/hash-prospector"
	| x |
	x := self.
	x := (x bitXor: (x bitShift: -17)) * 16red5ad4bb \\ 16r100000000.
    x := (x bitXor: (x bitShift: -11)) * 16rac4c1b51 \\ 16r100000000.
    x := (x bitXor: (x bitShift: -15)) * 16r31848bab \\ 16r100000000.
    x := x bitXor: (x bitShift: -14).
    ^ x
]

{ #category : '*Zag-Exporter' }
SmallInteger >> encodedBy: anASImage [

	^ anASImage objectSmallInteger: self
]
