Class {
	#name : #ASClosers,
	#superclass : #ASSyntax,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASClosers >> binaryExpression: anASCompiler target: target [
	^ target
]

{ #category : #parsing }
ASClosers >> keywordExpression: anASCompiler target: target [
	^ target
]

{ #category : #parsing }
ASClosers >> nonStatements: anASCompiler [ 

]

{ #category : #parsing }
ASClosers >> possibleAssign: anASCompiler target: target [ 
	^ target
]

{ #category : #parsing }
ASClosers >> unaryExpression: anASCompiler target: target [
	^ target
]
