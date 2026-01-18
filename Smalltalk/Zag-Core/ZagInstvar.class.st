Class {
	#name : 'ZagInstvar',
	#superclass : 'Object',
	#instVars : [
		'index',
		'basicBlock'
	],
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ZagInstvar >> basicBlock: aBasicBlock [

	basicBlock := aBasicBlock
]

{ #category : 'accessing' }
ZagInstvar >> index: anInteger [

	index := anInteger
]

{ #category : 'accessing' }
ZagInstvar >> name [
 ^ 'instanceVar index: ', index asString
]

{ #category : 'as yet unclassified' }
ZagInstvar >> pushOperation [

	^ ASCPushVariable with: self
]

{ #category : 'as yet unclassified' }
ZagInstvar >> pushOperationFrom: aBasicBlock [

	basicBlock := aBasicBlock.
	^ self halt
]

{ #category : 'output' }
ZagInstvar >> pushTheVariable: anOutput stackOffset: anInteger [

	^ anOutput 
]

{ #category : 'accessing' }
ZagInstvar >> stackValue [

	^ AStSubClass new
]
