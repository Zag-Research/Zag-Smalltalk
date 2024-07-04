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
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'compiling' }
ASInliner class >> lookup: aString [

	^ self lookup: aString stack: nil
]

{ #category : 'inlining' }
ASInliner class >> lookup: aSymbol class: aClass [

	^ map at: aSymbol->aClass ifAbsentPut: [
	(aClass whichClassIncludesSelector: aSymbol) ifNotNil: [ :class | class >> aSymbol ]]
]

{ #category : 'inlining' }
ASInliner class >> lookup: aString stack: aCollection [
	"	self halt."
	aString = #ifTrue:ifFalse: ifTrue: [ ^ { True>>#ifTrue:ifFalse: . False >>#ifTrue:ifFalse:}].
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
