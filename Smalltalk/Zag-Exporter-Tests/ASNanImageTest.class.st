Class {
	#name : 'ASNanImageTest',
	#superclass : 'ASImageTest',
	#pools : [
		'ASImageExporter'
	],
	#category : 'Zag-Exporter-Tests-Tests',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Tests'
}

{ #category : 'setup' }
ASNanImageTest >> encoderClass [

	^ ZagImageEncodeNan
]

{ #category : 'tests' }
ASNanImageTest >> testObjects [

	self
		image: [ :i | i encodingOf: 'Hello World!' ]
		equals: 16r7FFF000000100000
		withHeap: #( 16r270C0000010025 16r6F57206F6C6C6548
		   16r21646C72 ).
	self
		image: [ :i | i encodingOf: #( 4 2 ) ]
		equals: 16r7FFF000000100000
		withHeap: #( 16r27730000020027 16rFFFC000000000004 16rFFFC000000000002 ).
	self
		image: [ :i | i encodingOf: 4 @ 2 ]
		equals: 16r7FFF000000100000
		withHeap: #( 16r2770000003002F 16rFFFC000000000004 16rFFFC000000000002 ).
	self
		image: [ :i | i encodingOf: #( #( 4 2 ) 1 ) ]
		equals: 16r7FFF000000100000
		withHeap: #( 16r27730000040027 16r7fff000000100018 16rFFFC000000000001 16r27730000050027
			   16rFFFC000000000004 16rFFFC000000000002 )
]

{ #category : 'tests' }
ASNanImageTest >> testSimple [

	self image: [ :i | i encodingOf: 42 ] equals: 16rfff800000000002A.
	self image: [ :i | i encodingOf: 1 ] equals: 16rfff8000000000001.
	self image: [ :i | i encodingOf: -1 ] equals: 16rffffffffffffffff.
	self image: [ :i | i encodingOf: true ] equals: 16r7ffc000000000000.
	self image: [ :i | i encodingOf: false ] equals: 16r7ffb000000000000.
	self image: [ :i | i encodingOf: nil ] equals: 16r7FFF000000000000.
	self image: [ :i | i encodingOf: $A ] equals: 16r7FFD000000000041.
	self image: [ :i | i encodingOf: #value ] equals: 16r7FFA00003C6EF200.
	self image: [ :i | i encodingOf: #value: ] equals: 16r7FFA0000DAA66B01.
	self image: [ :i | i encodingOf: #value:value: ] equals: 16r7FFA0000E3779002.
	self image: [ :i | i encodingOf: 42.0 ] equals: 16r4045000000000000.
	self image: [ :i | i encodingOf: 2.0 ] equals: 16r4000000000000000.
	self image: [ :i | i encodingOf: 6.25e-2 ] equals: 16r3fb0000000000000.
	self image: [ :i | i encodingOf: #'==' ] equals: 16r7FFA0000A7089E01
]

{ #category : 'tests' }
ASNanImageTest >> test_simple1 [

	| ast |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple1.
	self threaded: ast equals: {
			#T_PushLiteral.
			42.
			#T_ReturnTop }
]

{ #category : 'tests' }
ASNanImageTest >> test_simple1_noreturn [

	| ast |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #simple1_noreturn.
	self threaded: ast equals: { #T_PushLiteral . 42 . #T_Drop . #T_ReturnSelf }
]
