"
I handle primitives

interpretFor: handles unary primitives

My subclasses are just to hande different arities
"
Class {
	#name : 'ASCPrimitive',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'primitive'
	],
	#classVars : [
		'Primitives'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'class initialization' }
ASCPrimitive class >> initialize [

	| primitives |
	primitives := ((Pragma all select: [ :p |
		                p selector = #primitive: and: [
			                p arguments first isInteger ] ]) collect: [ :p |
		               p arguments first -> (String streamContents: [ :s |
			                s
				                nextPutAll: #siPrim;
				                print: p arguments first.
			                (#( #':' with: with: with: with: ) first:
				                 p method selector numArgs) do: [ :e |
				                s nextPutAll: e ] ]) ]) asSet asArray sorted.
	Primitives := Array new: (primitives collect: #key) max.
	primitives do: [ :kv |
		(Primitives at: kv key)
			ifNil: [ Primitives at: kv key put: kv value asSymbol ]
			ifNotNil: [ :current |
				current size < kv value size ifTrue: [
					Primitives at: kv key put: kv value asSymbol ] ] ]
]

{ #category : 'instance creation' }
ASCPrimitive class >> with: anInteger [
	"	| primitive numArgs |
	primitive := Primitives at: anInteger.
	numArgs := primitive numArgs.
	numArgs = 2 ifTrue: [ ^ ASCPrimitive2 new with: primitive ].
	numArgs = 1 ifTrue: [ ^ ASCPrimitive1 new with: primitive ].
	numArgs = 0 ifTrue: [ "

	^ self new with: anInteger "primitive ].
	self error: 'only support 0, 1 and 2 arg primitives'"
]

{ #category : 'code generation' }
ASCPrimitive >> outputTo: output [

	^ output primitive: primitive
]

{ #category : 'stack' }
ASCPrimitive >> resultStack: aStack basicBlock: aBasicBlock [

	^ stack := aStack
]

{ #category : 'instance creation' }
ASCPrimitive >> with: aSymbol [

	primitive := aSymbol
]
