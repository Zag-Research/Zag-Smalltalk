Class {
	#name : 'ASMixImageTest',
	#superclass : 'ASImageTest',
	#category : 'Zag-Exporter-Tests-Tests',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Tests'
}

{ #category : 'setup' }
ASMixImageTest >> encoderClass [

	^ ZagImageEncodeMix
]

{ #category : 'tests' }
ASMixImageTest >> testObjects [

	self
		image: [ :i | i encodingOf: 'Hello World!' ]
		equals: 16r100000
		withHeap: #( 16r270C0000010025 16r6F57206F6C6C6548
		   16r21646C72 ).
	self
		image: [ :i | i encodingOf: #( 4 2 ) ]
		equals: 16r100000
		withHeap: #( 16r27730000020027 16r12 16rA ).
	self
		image: [ :i | i encodingOf: 4 @ 2 ]
		equals: 16r100000
		withHeap: #( 16r2770000003002F 16r12 16rA ).
	self
		image: [ :i | i encodingOf: #( #( 4 2 ) 1 ) ]
		equals: 16r100000
		withHeap: #( 16r27730000040027 16r100018 16r6 16r27730000050027
			   16r12 16rA )
]

{ #category : 'tests' }
ASMixImageTest >> testSimple [

	self image: [ :i | i encodingOf: 42 ] equals: 16r00000000000000aa.
	self image: [ :i | i encodingOf: 1 ] equals: 16r0000000000000006.
	self image: [ :i | i encodingOf: -1 ] equals: 16rfffffffffffffffe.
	self image: [ :i | i encodingOf: true ] equals: 16r6800000000000001.
	self image: [ :i | i encodingOf: false ] equals: 16r6000000000000001.
	self image: [ :i | i encodingOf: nil ] equals: 16r0000000000000000.
	self image: [ :i | i encodingOf: $A ] equals: 16r7000000000000209.
	self image: [ :i | i encodingOf: #Object ] equals: 16r58000000C17C0D01.
	self image: [ :i | i encodingOf: #value ] equals: 16r580000003C6EF201.
	self image: [ :i | i encodingOf: #value: ] equals: 16r58000000DAA66B11.
	self
		image: [ :i | i encodingOf: #value:value: ]
		equals: 16r58000000E3779021.
	self image: [ :i | i encodingOf: 42.0 ] equals: 16r08a0000000000005.
	self image: [ :i | i encodingOf: -1.0 ] equals: 16rfe00000000000014.
	self
		image: [ :i | i encodingOf: -6.25e-2 ]
		equals: 16rF600000000000014.
	self image: [ :i | i encodingOf: #'==' ] equals: 16r58000000A7089E11
]
