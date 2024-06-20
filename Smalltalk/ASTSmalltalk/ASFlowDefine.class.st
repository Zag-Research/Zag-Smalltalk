"
I am a flow source, but I'm a read-only value, so I'm given a value in the function header (if referenced at all).
I am common to all `ASCInlineBlock`s for a given `ASCReturnBlock` or `ASCMethodBlock`
"
Class {
	#name : 'ASFlowDefine',
	#superclass : 'ASFlowNil',
	#instVars : [
		'referenced'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'initialization' }
ASFlowDefine >> basicBlock: anASCInlineBlock [ 

		id := anASCInlineBlock nextSequence asString
]

{ #category : 'initialization' }
ASFlowDefine >> initialize [

	super initialize.
	referenced := false
]

{ #category : 'testing' }
ASFlowDefine >> isNilReference [

	^ false
]

{ #category : 'data flow instructions' }
ASFlowDefine >> isReferenced [

	^ referenced
]

{ #category : 'data flow instructions' }
ASFlowDefine >> markReferenced [

	referenced := true
]
