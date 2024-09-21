Class {
	#name : 'ASEnd',
	#superclass : 'ASClosers',
	#category : 'ASTSmalltalk-Tokens',
	#package : 'ASTSmalltalk',
	#tag : 'Tokens'
}

{ #category : 'testing' }
ASEnd >> atEnd [
	^ true
]

{ #category : 'parsing' }
ASEnd >> expectEnd [
]

{ #category : 'parsing' }
ASEnd >> statement: anASCompiler [ 
	^ nil
]
