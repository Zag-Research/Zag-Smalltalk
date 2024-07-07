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

{ #category : 'operations' }
ASLocalNamed >> mergeWith: aCollection [

	source := source mergeWith: aCollection source
]

{ #category : 'data flow instructions' }
ASLocalNamed >> newElementFor: aBasicBlock [

	^ (self replicate: (next newElementFor: aBasicBlock)) source: aBasicBlock classForLocals new
]

{ #category : 'data flow instructions' }
ASLocalNamed >> simplifyDependenciesFor: aBlock [

	next simplifyDependenciesFor: aBlock.
	source isPhiReference ifTrue: [
		(aBlock anySourcesNonNilFor: self id) ifFalse: [
			source := ASFlowNil singleton ] ]
]
