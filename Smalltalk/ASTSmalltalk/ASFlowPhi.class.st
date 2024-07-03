"
I am a Phi node, which means I get values from other blocks.
If all predecessor blocks have the same value, I will represent that.
If any are different, then I will generate a Phi node for LLVM to allocate.
"
Class {
	#name : 'ASFlowPhi',
	#superclass : 'ASFlowNonNil',
	#instVars : [
		'sources'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'instance creation' }
ASFlowPhi >> from: anASFlow [

	sources add: anASFlow
]

{ #category : 'initialization' }
ASFlowPhi >> initialize [

	super initialize.
	sources := OrderedCollection new
]

{ #category : 'testing' }
ASFlowPhi >> isNilReference [

	^ sources allSatisfy: [ :each | each isNilReference ]
]

{ #category : 'testing' }
ASFlowPhi >> isPhiReference [

	^ true
]

{ #category : 'operations' }
ASFlowPhi >> mergeWith: anASFlow [

	sources add: anASFlow
]

{ #category : 'printing' }
ASFlowPhi >> printOn: aStream [

	^ aStream nextPutAll: 'phi'
]
