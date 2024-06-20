"
I am a flow source, but I'm `nil` right now.
I am the initial source for a local variable.
"
Class {
	#name : 'ASFlowNil',
	#superclass : 'Object',
	#instVars : [
		'id'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASFlowNil >> id [

	^ id
]

{ #category : 'accessing' }
ASFlowNil >> id: aString [ 
	id := aString
]

{ #category : 'testing' }
ASFlowNil >> isNilReference [

	^ true
]

{ #category : 'testing' }
ASFlowNil >> isPhiReference [

	^ false
]

{ #category : 'data flow instructions' }
ASFlowNil >> isReferenced [

	^ false
]

{ #category : 'data flow instructions' }
ASFlowNil >> markReferenced [
]
