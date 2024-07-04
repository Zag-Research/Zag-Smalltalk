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
		assert: (ASInliner lookup: #'<=' class: SmallInteger)
		equals: SmallInteger >> #'<='.
	self deny: (ASInliner lookup: #ifTrue:ifFalse:) equals: nil
]
