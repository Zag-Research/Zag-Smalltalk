Class {
	#name : 'ASNanImageTest',
	#superclass : 'ASImageTest',
	#pools : [
		'ASZagConstants'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'as yet unclassified' }
ASNanImageTest >> imageClass [
	^ ASNanImage
]

{ #category : 'tests' }
ASNanImageTest >> testCompile [

	| ast |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple1.
	self threaded: ast equals: #( pushLiteral 42 returnTop )
]

{ #category : 'tests' }
ASNanImageTest >> testObjects [

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
ASNanImageTest >> testSimple [

	self image: [ :i | i put: 42 ] equals: 16rfffc00000000002a.
	self image: [ :i | i put: 1 ] equals: 16rfffc000000000001.
	self image: [ :i | i put: -1 ] equals: 16rfffbffffffffffff.
	self image: [ :i | i put: true ] equals: 16rfff0000500000001.
	self image: [ :i | i put: false ] equals: 16rfff0000400000000.
	self image: [ :i | i put: nil ] equals: 16rfff00003ffffffff.
	self image: [ :i | i put: $A ] equals: 16rfff0000800000041.
	self image: [ :i | i put: #value ] equals: 16rfff0000700000200.
	self image: [ :i | i put: #value: ] equals: 16rfff0000700000301.
	self image: [ :i | i put: #value:value: ] equals: 16rfff0000700001002.
	self image: [ :i | i put: 42.0 ] equals: 16r4045000000000000.
	self image: [ :i | i put: 2.0 ] equals: 16r4000000000000000.
	self image: [ :i | i put: 6.25e-2 ] equals: 16r3fb0000000000000.
	self image: [ :i | i put: #'==' ] equals: 16rfff0000700000E01
]

{ #category : 'tests' }
ASNanImageTest >> testU64From [

	self assert: (self u64From: #[  ]) equals: #(  ).
	self assert: (self u64From: #[ 1 ]) equals: #( 1 ).
	self
		assert: (self u64From: #[ 1 2 3 4 5 6 7 8 ])
		equals: #( 16r0807060504030201 ).
	self
		assert: (self u64From: #[ 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8])
		equals: #( 16r0807060504030201 16r0605040302010009 16r0807 )
]
