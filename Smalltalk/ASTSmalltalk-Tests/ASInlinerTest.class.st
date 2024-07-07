Class {
	#name : 'ASInlinerTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASInlinerTest >> setUp [
	super setUp.

	ASInliner reset
]

{ #category : 'tests' }
ASInlinerTest >> testInitial [

	self assert: (ASInliner lookup: #printOn:) equals: nil.
	self
		assert: (ASInliner lookup: #'<=' class: SmallInteger) methods
		equals: { (ASTSmallInteger >> #'<=') }.
	self
		assert: (ASInliner lookup: #ifTrue:ifFalse:) methods asSet
		equals: {
				(ASTFalse >> #ifTrue:ifFalse:).
				(ASTTrue >> #ifTrue:ifFalse:) } asSet.
	self
		assert: (ASInliner lookup: #yourself class: SmallInteger) methods
		equals: { (ASTObject >> #yourself) }
]
