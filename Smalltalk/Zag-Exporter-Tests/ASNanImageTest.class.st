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

{ #category : 'as yet unclassified' }
ASNanImageTest >> encoderClass [

	^ ZagImageEncodeNan
]

{ #category : 'tests' }
ASNanImageTest >> testObjects [

	self
		image: [ :i | i put: 'Hello' ]
		equals: #( 16r1705000001001F 16r6F6C6C6548 16rFFF7000000100020 ).
	self
		image: [ :i | i put: #( 4 2 ) ]
		equals:
		#( 16r27730000020009 16rfffc000000000004 16rfffc000000000002 16rFFF7000000100020 ).
	self
		image: [ :i | i put: 4 @ 2 ]
		equals:
		#( 16r2770000003001D 16rfffc000000000004 16rfffc000000000002 16rFFF7000000100020 ).
	self
		image: [ :i | i put: #( #( 4 2 ) 1 ) ]
		equals:
			#( 16r27730000040009 16rfff7000000100038 16rfffc000000000001
			   16r27730000050009 16rfffc000000000004 16rfffc000000000002 16rFFF7000000100020 )
]

{ #category : 'tests' }
ASNanImageTest >> testSimple [

	self image: [ :i | i put: 42 ] equals: 16rfffc00000000002a.
	self image: [ :i | i put: 1 ] equals: 16rfffc000000000001.
	self image: [ :i | i put: -1 ] equals: 16rfffbffffffffffff.
	self image: [ :i | i put: true ] equals: 16rfff0000B00000001.
	self image: [ :i | i put: false ] equals: 16rfff0000A00000000.
	self image: [ :i | i put: nil ] equals: 16rfff00017ffffffff.
	self image: [ :i | i put: $A ] equals: 16rfff0000E00000041.
	self image: [ :i | i put: #value ] equals: 16rfff0000D00000200.
	self image: [ :i | i put: #value: ] equals: 16rfff0000D00000301.
	self image: [ :i | i put: #value:value: ] equals: 16rfff0000D00001002.
	self image: [ :i | i put: 42.0 ] equals: 16r4045000000000000.
	self image: [ :i | i put: 2.0 ] equals: 16r4000000000000000.
	self image: [ :i | i put: 6.25e-2 ] equals: 16r3fb0000000000000.
	self image: [ :i | i put: #'==' ] equals: 16rfff0000D00000E01
]

{ #category : 'tests' }
ASNanImageTest >> test_simple1 [

	| ast |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple1.
	self threaded: ast equals: {
			constants tPushLiteral.
			42.
			constants tReturnTop }
]

{ #category : 'tests' }
ASNanImageTest >> test_simple1_noreturn [

	| ast |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #simple1_noreturn.
	self threaded: ast equals: { constants tPushLiteral . 42 . constants tDrop . constants tReturnSelf }
]
