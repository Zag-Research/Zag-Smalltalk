Class {
	#name : 'ASTextFileContext',
	#superclass : 'ASFileContext',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'converting' }
ASTextFileContext >> asString [
^ String streamContents: [ :s | self outputTo: s ]
]

{ #category : 'code generation' }
ASTextFileContext >> outputTo: stream [

	^ self subclassResponsibility 
]
