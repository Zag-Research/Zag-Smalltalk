"
I have subclasses for all the statments that can exist in the body of a method or a block.
"
Class {
	#name : #ASStatement,
	#superclass : #Object,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #testing }
ASStatement >> isASLiteral [
	^false
]

{ #category : #testing }
ASStatement >> isASRef [
	^false
]

{ #category : #testing }
ASStatement >> isASSelf [
	^false
]

{ #category : #testing }
ASStatement >> isASSend [
	^false
]
