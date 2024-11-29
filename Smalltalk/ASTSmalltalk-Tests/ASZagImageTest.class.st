Class {
	#name : 'ASZagImageTest',
	#superclass : 'ASImageTest',
	#pools : [
		'ASImageExporter',
		'ASZagConstants'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASZagImageTest >> imageClass [

	^ ASZagImage
]

{ #category : 'tests' }
ASZagImageTest >> testObjects [

	self
		image: [ :i | i put: 'Hello' ]
		equals: #( 16r1705000001000A 16r6F6C6C6548 ).
	self
		image: [ :i | i put: #( 4 2 ) ]
		equals:
		#( 16r27730000020009 16rfffc000000000004 16rfffc000000000002 ).
	self
		image: [ :i | i put: 4 @ 2 ]
		equals:
		#( 16r2770000003001D 16rfffc000000000004 16rfffc000000000002 ).
	self
		image: [ :i | i put: #( #( 4 2 ) 1 ) ]
		equals:
			#( 16r27730000040009 16rfff7001000000018 16rfffc000000000001
			   16r27730000050009 16rfffc000000000004 16rfffc000000000002 )
]

{ #category : 'tests' }
ASZagImageTest >> testSimple [

	self image: [ :i | i put: 42 ] equals: 16r0000000000002a61.
	self image: [ :i | i put: 1 ] equals:  16r0000000000000161.
	self image: [ :i | i put: -1 ] equals: 16rffffffffffffff61.
	self image: [ :i | i put: true ] equals: 16r0000000000000059.
	self image: [ :i | i put: false ] equals: 16r0000000000000051.
	self image: [ :i | i put: nil ] equals: 16r0000000000000000.
	self image: [ :i | i put: $A ] equals: 16r0000000000004171.
	self image: [ :i | i put: #value ] equals: 16r0000000000020069.
	self image: [ :i | i put: #value: ] equals: 16r0000000000030169.
	self image: [ :i | i put: #value:value: ] equals: 16r0000000000100269.
	self image: [ :i | i put: 42.0 ] equals: 16r450000000000006.
	self image: [ :i | i put: 2.0 ] equals: 16r0000000000000006.
	self image: [ :i | i put: -6.25e-2 ] equals: 16rfb0000000000000d.
	self image: [ :i | i put: #'==' ] equals: 16r00000000000E0169
]
