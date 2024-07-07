"
I keep track of potential inlining opportunities
"
Class {
	#name : 'ASInliner',
	#superclass : 'Object',
	#instVars : [
		'implementors'
	],
	#classInstVars : [
		'map'
	],
	#category : 'ASTSmalltalk-Inlining',
	#package : 'ASTSmalltalk',
	#tag : 'Inlining'
}

{ #category : 'as yet unclassified' }
ASInliner class >> filterMethods: aCollection [

	| zagOverridden |
	zagOverridden := Set new.
	aCollection do: [ :method |
		| cName |
		cName := method methodClass name.
		(cName beginsWith: 'AST') ifTrue: [
			zagOverridden add: (cName allButFirst: 3) ] ].
	^ aCollection select: [ :method |
		method isRequired not and: [
		  | cName |
		  cName := method methodClass name.
		  (cName beginsWith: 'AST') or: [ (zagOverridden includes: cName) not ] ] ]
]

{ #category : 'inlining' }
ASInliner class >> findImplementations: aSymbol [

	| implementors |
	implementors := self filterMethods: aSymbol implementors.
	(implementors size between: 1 and: 4) ifFalse: [ ^ nil ].
	^ ASInlineNotExhaustive new methods: implementors
]

{ #category : 'inlining' }
ASInliner class >> findImplementations: aSymbol class: aClass stack: aStack [

	aClass ifNil: [ ^ self findImplementations: aSymbol ].
	(aClass whichClassIncludesSelector: aSymbol) ifNotNil: [ :class |
		| astClass |
		astClass := class astClass.
		(astClass includesSelector: aSymbol) ifFalse: [ astClass := class ].
		^ ASInlineSingleton new method: (astClass >> aSymbol) class: aClass ].
	^ nil
]

{ #category : 'compiling' }
ASInliner class >> lookup: aString [

	^ self lookup: aString class: nil
]

{ #category : 'inlining' }
ASInliner class >> lookup: aSymbol class: aClass [

	^ map
		  at: aSymbol -> aClass
		  ifAbsentPut: [
		  self findImplementations: aSymbol class: aClass stack: nil ]
]

{ #category : 'inlining' }
ASInliner class >> lookup: selector stack: aStack [
	(self lookup: selector class: (aStack at: selector numArgs) valueClass) ifNotNil: [ :v| ^ v ].
self halt.
	^ nil
]

{ #category : 'class initialization' }
ASInliner class >> reset [

	(map := LRUCache new) maximumWeight: 100
]

{ #category : 'initialization' }
ASInliner >> initialize [

	super initialize.
	implementors := Dictionary new.
	implementors at: #not put: {
			(True >> #not).
			(False >> #not) }.
	implementors at: #ifTrue: put: {
			(True >> #ifTrue:).
			(False >> #ifTrue:) }.
	implementors at: #ifFalse: put: {
			(True >> #ifFalse:).
			(False >> #ifFalse:) }.
	implementors at: #ifTrue:ifFalse: put: {
			(True >> #ifTrue:ifFalse:).
			(False >> #ifTrue:ifFalse:) }.
	implementors at: #ifFalse:ifTrue: put: {
			(True >> #ifFalse:ifTrue:).
			(False >> #ifFalse:ifTrue:) }.
	implementors at: #isLarge put: { (SmallInteger >> #isLarge) }.
	implementors at: #not put: { (Number >> #abs) }.
	implementors at: #value put: { (ASTBlockClosure >> #value) }.
	implementors at: #whileTrue put: { (ASTBlockClosure >> #whileTrue) }.
	implementors
		at: #fibonacci_fast
		put: { (Integer >> #fibonacci_fast) }.
	implementors
		at: #fibonacci_accumulator:prev:
		put: { (Integer >> #fibonacci_accumulator:prev:) }
]
