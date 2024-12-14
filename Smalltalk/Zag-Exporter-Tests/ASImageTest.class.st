Class {
	#name : 'ASImageTest',
	#superclass : 'TestCase',
	#instVars : [
		'constants'
	],
	#category : 'Zag-Exporter-Tests-Tests',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Tests'
}

{ #category : 'setup' }
ASImageTest >> encoderClass [

	^ nil
]

{ #category : 'running' }
ASImageTest >> image: aBlock equals: value [

	| result image |
	image := ASImageExporter test constants: constants.
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

{ #category : 'running' }
ASImageTest >> setUp [

	super setUp.
	ASAllocationInfo reset.
	constants := self encoderClass
]

{ #category : 'tests' }
ASImageTest >> testObjects [
	^ self subclassResponsibility
]

{ #category : 'tests' }
ASImageTest >> testSimple [
	^ self subclassResponsibility
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
	image := ASImageExporter test constants: constants; on: (ReadWriteStream on: #[  ]).
	ASImageThreadedOutput new
		constants: constants;
		context: (ASImageFileContext with: image);
		generateCode: ast.
	converted withIndexDo: [ :each :idx |
		| code |
		code := image rawIntAt: idx.
		each isSymbol
			ifTrue: [
				self
					assert: code = (constants perform: each)
					description: [
					'expected: ' , each , ' but found: ' , code hexString ] ]
			ifFalse: [
				each isNumber
					ifTrue: [
					self assert: code equals: (self imageClass convertNumber: each) ]
					ifFalse: [ self assert: code equals: each ] ] ]
]
