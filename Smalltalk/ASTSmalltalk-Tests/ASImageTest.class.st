Class {
	#name : 'ASImageTest',
	#superclass : 'TestCase',
	#pools : [
		'ASZagConstants'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASImageTest >> image: aBlock equals: value [

	| result image |
	image := self imageClass test.
	result := ByteArray streamContents: [ :s |
		          aBlock value: image.
		          image currentHeap toStream: s ].
	self
		assert: result asU64Array first hexString
		equals: image currentHeap address hexString.
	value isInteger
		ifTrue: [
			self
				assert: result asU64Array fourth hexString
				equals: value hexString ]
		ifFalse: [
			self
				assert: ((result asU64Array allButFirst: 3) collect: #hexString)
				equals: (value collect: #hexString) ]
]

{ #category : 'tests' }
ASImageTest >> imageClass [

	^ self subclassResponsibility
]

{ #category : 'running' }
ASImageTest >> setUp [

	super setUp.

	ASAllocationInfo reset
]

{ #category : 'tests' }
ASImageTest >> testU64Array [

	self assert: #[  ] asU64Array equals: #(  ).
	self assert: #[ 1 ] asU64Array equals: #( 1 ).
	self
		assert: #[ 1 2 3 4 5 6 7 8 ] asU64Array
		equals: #( 16r0807060504030201 ).
	self
		assert: #[ 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 ] asU64Array
		equals: #( 16r0807060504030201 16r0605040302010009
		   16r0807 )
]

{ #category : 'tests' }
ASImageTest >> threaded: ast equals: converted [

	| image |
	image := self imageClass test on: (ReadWriteStream on: #[  ]).
	ASImageThreadedOutput new
		context: (ASImageFileContext with: image);
		generateCode: ast.
	converted withIndexDo: [ :each :idx |
		| code |
		code := image rawIntAt: idx.
		each isSymbol
			ifTrue: [
				self
					assert: code = (ASZagConstants classVarNamed: each)
					description: [
					'expected: ' , each , ' but found: ' , code hexString ] ]
			ifFalse: [
				each isNumber
					ifTrue: [
					self assert: code equals: (self imageClass convertNumber: each) ]
					ifFalse: [ self assert: code equals: each ] ] ]
]
