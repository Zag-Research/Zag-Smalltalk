Class {
	#name : 'ASZagImageTest',
	#superclass : 'ASImageTest',
	#pools : [
		'ASImageExporter'
	],
	#classInstVars : [
		'zagConstants'
	],
	#category : 'Zag-Exporter-Tests-Tests',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Tests'
}

{ #category : 'setup' }
ASZagImageTest >> encoderClass [

	^ ZagImageEncodeZag
]

{ #category : 'tests' }
ASZagImageTest >> testObjects [

	self
		image: [ :i | i encodingOf: 'Hello World!' ]
		equals: 16r100088
		withHeap: #( 16r270C0000010028 16r6F57206F6C6C6548
		   16r21646C72 ).
	self
		image: [ :i | i encodingOf: #( 4 2 ) ]
		equals: 16r100088
		withHeap: #( 16r27730000020027 16r481 16r281 ).
	self
		image: [ :i | i encodingOf: 4 @ 2 ]
		equals: 16r100088
		withHeap: #( 16r2770000003002F 16r481 16r281 ).
	self
		image: [ :i | i encodingOf: #( #( 4 2 ) 1 ) ]
		equals: 16r100088
		withHeap: #( 16r27730000040027 16r1000A0 16r181 16r27730000050027
			   16r481 16r281 )
]

{ #category : 'tests' }
ASZagImageTest >> testSimple [

	self image: [ :i | i encodingOf: 42 ] equals: 16r0000000000002a81.
	self image: [ :i | i encodingOf: 1 ] equals: 16r0000000000000181.
	self image: [ :i | i encodingOf: -1 ] equals: 16rffffffffffffff81.
	self image: [ :i | i encodingOf: true ] equals: 16r0000000000000079.
	self image: [ :i | i encodingOf: false ] equals: 16r0000000000000071.
	self image: [ :i | i encodingOf: nil ] equals: 16r0000000000000000.
	self image: [ :i | i encodingOf: $A ] equals: 16r0000000000004191.
	self image: [ :i | i encodingOf: #Object ] equals: 16r5FB38689.
	self image: [ :i | i encodingOf: #value ] equals: 16r3C6EF289.
	self image: [ :i | i encodingOf: #value: ] equals: 16r1DAA66B89.
	self
		image: [ :i | i encodingOf: #value:value: ]
		equals: 16r2E3779089.
	self image: [ :i | i encodingOf: 42.0 ] equals: 16r450000000000006.
	self image: [ :i | i encodingOf: 2.0 ] equals: 16r0000000000000006.
	self
		image: [ :i | i encodingOf: -6.25e-2 ]
		equals: 16rfb0000000000000d.
	self image: [ :i | i encodingOf: #'==' ] equals: 16r1A7089E89
]
