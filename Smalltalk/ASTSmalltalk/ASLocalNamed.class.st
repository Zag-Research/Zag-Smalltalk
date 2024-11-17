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

{ #category : 'accessing' }
ASLocalNamed >> countParameters [

	^ next countParameters
]

{ #category : 'instance creation' }
ASLocalNamed >> fromStack: anASStack [

	offset := anASStack offset.
	next := anASStack
]

{ #category : 'testing' }
ASLocalNamed >> isLocal [

	^ true
]

{ #category : 'operations' }
ASLocalNamed >> mergeWith: aStackElement [

	location stackValue: (location stackValue mergeWith: aStackElement stackValue)
]
