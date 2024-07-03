Class {
	#name : 'ASInliner',
	#superclass : 'Object',
	#instVars : [
		'implementors'
	],
	#classInstVars : [
		'soleInstance'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASInliner class >> soleInstance [

	^ soleInstance ifNil: [ soleInstance := self new ]
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

{ #category : 'inlining' }
ASInliner >> lookup: aString stack: aCollection [

	^ nil
]
