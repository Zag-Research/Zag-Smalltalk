"
I am a named stack variable but am found in a parents stack.
"
Class {
	#name : 'ASLocalNamed',
	#superclass : 'ASStackNamed',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'data flow instructions' }
ASLocalNamed >> newElementFor: aBasicBlock [

	| new |
	self halt.
	new := aBasicBlock classForLocals new id: name.
	^ (self replicate: (rest newElementFor: aBasicBlock)) source: new
]

{ #category : 'data flow instructions' }
ASLocalNamed >> simplifyDependenciesFor: aBlock [

	rest simplifyDependenciesFor: aBlock.
	source isPhiReference ifTrue: [
		(aBlock anySourcesNonNilFor: self id) ifFalse: [
			source := ASFlowNil new ] ]
]
