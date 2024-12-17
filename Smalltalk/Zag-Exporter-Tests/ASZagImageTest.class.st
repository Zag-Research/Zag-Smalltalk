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
		equals: 16r100020 withHeap:
		#( 16r270C0000010028 16r6F57206F6C6C6548 16r21646C72 16r100020 ).
	self
		image: [ :i | i encodingOf: #( 4 2 ) ]
		equals: 16r100020 withHeap: #( 16r27730000020027 16r471 16r271 16r100020 ).
	self
		image: [ :i | i encodingOf: 4 @ 2 ]
		equals: 16r100020 withHeap: #( 16r2770000003002F 16r471 16r271 16r100020 ).
	self
		image: [ :i | i encodingOf: #( #( 4 2 ) 1 ) ]
		equals: 16r100020 withHeap:
			#( 16r27730000040027 16r100038 16r171 16r27730000050027
			   16r471 16r271 16r100020 )
]

{ #category : 'tests' }
ASZagImageTest >> testSimple [

	self image: [ :i | i encodingOf: 42 ] equals: 16r0000000000002a71.
	self image: [ :i | i encodingOf: 1 ] equals: 16r0000000000000171.
	self image: [ :i | i encodingOf: -1 ] equals: 16rffffffffffffff71.
	self image: [ :i | i encodingOf: true ] equals: 16r0000000000000069.
	self image: [ :i | i encodingOf: false ] equals: 16r0000000000000061.
	self image: [ :i | i encodingOf: nil ] equals: 16r0000000000000000.
	self image: [ :i | i encodingOf: $A ] equals: 16r0000000000004181.
	self image: [ :i | i encodingOf: #value ] equals: 16r0000000000020079.
	self image: [ :i | i encodingOf: #value: ] equals: 16r0000000000030179.
	self image: [ :i | i encodingOf: #value:value: ] equals: 16r0000000000100279.
	self image: [ :i | i encodingOf: 42.0 ] equals: 16r450000000000006.
	self image: [ :i | i encodingOf: 2.0 ] equals: 16r0000000000000006.
	self image: [ :i | i encodingOf: -6.25e-2 ] equals: 16rfb0000000000000d.
	self image: [ :i | i encodingOf: #'==' ] equals: 16r00000000000E0179
]
