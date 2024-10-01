"
Placeholder for an `ASContext`

The size is the offset between the last named element and the first temporary
"
Class {
	#name : 'ASStackContext',
	#superclass : 'ASStack',
	#instVars : [
		'size',
		'next'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASStackContext >> at: aSymbol [

	^ next at: aSymbol
]

{ #category : 'instance creation' }
ASStackContext >> fromStack: anASStack [

	next := anASStack
]

{ #category : 'initialization' }
ASStackContext >> initialize [

	size := 0
]

{ #category : 'accessing' }
ASStackContext >> size [

	^ size
]
