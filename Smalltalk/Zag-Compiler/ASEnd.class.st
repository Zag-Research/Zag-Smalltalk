Class {
	#name : 'ASEnd',
	#superclass : 'ASClosers',
	#category : 'Zag-Compiler-Tokens',
	#package : 'Zag-Compiler',
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
