Class {
	#name : 'ASClosers',
	#superclass : 'ASSyntax',
	#category : 'Zag-Compiler-Tokens',
	#package : 'Zag-Compiler',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASClosers >> binaryExpression: anASCompiler target: target [
	^ target
]

{ #category : 'parsing' }
ASClosers >> keywordExpression: anASCompiler target: target [
	^ target
]

{ #category : 'parsing' }
ASClosers >> keywordExpression: anASCompiler target: target selector: selector args: args [

	^ ASSend new target: target; selector: selector args: args
]

{ #category : 'parsing' }
ASClosers >> nonStatements: anASCompiler [ 

]

{ #category : 'parsing' }
ASClosers >> possibleAssign: anASCompiler target: target [ 
	^ target
]

{ #category : 'parsing' }
ASClosers >> unaryExpression: anASCompiler target: target [
	^ target
]
