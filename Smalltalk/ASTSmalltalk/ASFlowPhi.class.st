"
I am a Phi node, which means I get values from other blocks.
If all predecessor blocks have the same value, I will represent that.
If any are different, then I will generate a Phi node for LLVM to allocate.
"
Class {
	#name : 'ASFlowPhi',
	#superclass : 'ASFlowNew',
	#instVars : [
		'sources'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASFlowPhi >> dataClass [

	| aClass |
	sources do: [ :each |
		aClass
			ifNil: [ aClass := each dataClass ]
			ifNotNil: [ aClass = each dataClass ifFalse: [ ^ nil ] ] ].
	^ aClass
]

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

{ #category : 'testing' }
ASFlowPhi >> needsToBeSpilled [

	^ true
]

{ #category : 'printing' }
ASFlowPhi >> printOn: aStream [

	^ sources size = 1 ifTrue: [ sources first printOn: aStream ] ifFalse: [ aStream nextPutAll: 'phi']
]
