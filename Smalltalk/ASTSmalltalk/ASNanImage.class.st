"
I write out a NaN-encoded image
"
Class {
	#name : 'ASNanImage',
	#superclass : 'ASImage',
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'class initialization' }
ASNanImage class >> initialize [

	SizeMask := 16r7fff.
	SizeShift := 48.
	FormatMask := 16r1f.
	FormatShift := 40.
	HashShift := 20.
	HashMask := 1 << (FormatShift - HashShift) - 1.
	ClassMask := 1 << HashShift - 1
]

{ #category : 'adding' }
ASNanImage >> header: header [
	| code key value |
	code := 0.
	header do: [: flag |
		flag isAssociation ifTrue: [
			key := flag key.
			value := flag value.
			key = #class ifTrue: [ code := code bitOr: ((self class: value) bitAnd: ClassMask) . key := nil ].
			key = #hash ifTrue: [ code := code bitOr: (value bitAnd: HashMask) << HashShift . key := nil ].
			key = #size ifTrue: [ code := code bitOr: (value bitAnd: SizeMask) << SizeShift . key := nil ].
			key = #format ifTrue: [ code := code bitOr: (value bitAnd: FormatMask) << FormatShift . key := nil ].
		] ifFalse: [
			key := flag.
			key = #isImmutable ifTrue: [ code := code bitOr: 1 << 47 . key := nil ].
			key = #isForwarded ifTrue: [ code := code bitOr: 1 << 63 . key := nil ].
		].
		key ifNotNil: [ self error: 'header field "',key asString,'" is illegal' ]
	].
	^ code
]

{ #category : 'adding' }
ASNanImage >> include: anASGenerator object: anObject [
	| nVars extra size header format fields |
	fields := anObject astFields.
	nVars := fields size.
	header := OrderedCollection with: #class -> anObject class.
	header add: #hash -> currentAddress.
	format := anObject astFormat.
	header add: #format -> format.
	extra := nil.
	(format & 24)>0 ifTrue: [ 
			(format & 17) = 1 ifTrue: [ 
					self error: 'indexable with instVars' ]
				ifFalse: [ 
					nVars <= SizeMask
						ifTrue: [ header add: #size -> nVars ]
						ifFalse: [
							extra := nVars.
							header add: #size -> SizeMask.
							self error: 'oversize array' ] ]
				  ]
		ifFalse: [ header add: #size -> nVars ].
	size := anASGenerator
		address: currentAddress
		extra: extra
		header: (self header: header)
		fields: fields.
	currentAddress := currentAddress + (size * 8)
]

{ #category : 'adding' }
ASNanImage >> literal: l class: c [
	^ 16r7FF8000000000000 + (l bitShift: 3) + c
]

{ #category : 'adding' }
ASNanImage >> putFloat: aFloat [

	aStream nextNumber: 4 put: (aFloat at: 1).
	^ aStream nextNumber: 4 put: (aFloat at: 2)
]

{ #category : 'adding' }
ASNanImage >> putSmallInteger: anInteger [

	^ self halt. "literal: self class: 5"
]
