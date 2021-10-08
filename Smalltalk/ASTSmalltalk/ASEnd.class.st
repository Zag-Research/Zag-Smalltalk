Class {
	#name : #ASEnd,
	#superclass : #ASToken,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #testing }
ASEnd >> atEnd [
	^ true
]

{ #category : #parsing }
ASEnd >> expectEnd [
]

{ #category : #parsing }
ASEnd >> keywordExpression: anASCompiler target: target [ 
	^ target
]

{ #category : #parsing }
ASEnd >> nonStatements: anASCompiler [ 

]
