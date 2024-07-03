"
I am a flow source in a ASReturnBlock so will be loaded from the stack, but I reference the original flow source so that things will be put on the stack before the call
"
Class {
	#name : 'ASFlowFromStack',
	#superclass : 'ASFlowNonNil',
	#instVars : [
		'from'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASFlowFromStack >> from [

	^ from
]

{ #category : 'accessing' }
ASFlowFromStack >> from: aFlow [

	id := aFlow id.
	from := aFlow
]

{ #category : 'data flow instructions' }
ASFlowFromStack >> markReferenced [

	from markReferenced.
	^ super markReferenced
]
