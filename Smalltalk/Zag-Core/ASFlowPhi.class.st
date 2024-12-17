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
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
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

{ #category : 'output' }
ASFlowPhi >> getNameFor: aStackElement on: anOutput [

	^ outputName ifNil: [ outputName := anOutput generatePhi: aStackElement ]
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

{ #category : 'as yet unclassified' }
ASFlowPhi >> mergeMultiple: sourceList [ 

	sourceList do: [ :source | self mergeWith: source flow. ]
]

{ #category : 'operations' }
ASFlowPhi >> mergeWith: anASFlow [

	sources add: anASFlow
]

{ #category : 'printing' }
ASFlowPhi >> printOn: aStream [

	sources size = 1
		ifTrue: [
			aStream
				nextPut: $?;
				print: sources first ]
		ifFalse: [
			sources size = 0 ifTrue: [ ^ super printOn: aStream ] ifFalse: [
			aStream nextPutAll: '?('.
			sources
				do: [ :source | source printOn: aStream ]
				separatedBy: [ aStream space ].
			aStream nextPut: $) ]].
	^ self printClassOn: aStream
]
