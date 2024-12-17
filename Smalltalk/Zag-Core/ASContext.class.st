"
I rempresent a stack context.

I may,  or may not be reified in a particular method
"
Class {
	#name : 'ASContext',
	#superclass : 'Object',
	#instVars : [
		'uses',
		'previous',
		'basicBlock'
	],
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
ASContext class >> forBasicBlock: aBasicBlock [

	| anASContext |
	anASContext := self new.

	anASContext forBasicBlock: aBasicBlock
]

{ #category : 'as yet unclassified' }
ASContext >> addUse [

	uses := uses + 1
]

{ #category : 'accessing' }
ASContext >> basicBlock [

	^ basicBlock
]

{ #category : 'displaying' }
ASContext >> displayStringOn: stream [

	self printNameOn: stream
]

{ #category : 'enumerating' }
ASContext >> do: aBlock [

	self flag: #ToDo.
	self error: ' this method should go away... Pharo bug wouldn''t let me remove '
]

{ #category : 'instance creation' }
ASContext >> forBasicBlock: aBasicBlock [

	basicBlock ifNil: [
		basicBlock := aBasicBlock.
		^ self ].
	aBasicBlock == basicBlock ifTrue: [ ^ self ].
	self halt.
	^ (self class forBasicBlock: aBasicBlock) previous: self
]

{ #category : 'initialization' }
ASContext >> initialize [

	super initialize.
	uses := 0
]

{ #category : 'displaying' }
ASContext >> printNameOn: stream [

	stream nextPutAll: '<context>'
]

{ #category : 'as yet unclassified' }
ASContext >> removeUse [

	uses := uses - 1
]
