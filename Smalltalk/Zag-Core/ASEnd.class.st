Class {
	#name : 'ASEnd',
	#superclass : 'ASClosers',
	#category : 'Zag-Core-Tokens',
	#package : 'Zag-Core',
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
