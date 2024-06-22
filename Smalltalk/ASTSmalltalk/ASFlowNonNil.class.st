"
I am an abstract flow source
"
Class {
	#name : 'ASFlowNonNil',
	#superclass : 'ASFlowNil',
	#instVars : [
		'id'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASFlowNonNil >> id [

	^ id
]

{ #category : 'accessing' }
ASFlowNonNil >> id: aString [ 
	id := aString
]

{ #category : 'testing' }
ASFlowNonNil >> isNilReference [

	^ false
]
