Class {
	#name : #ASKeywordId,
	#superclass : #ASIdentifier,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #accessing }
ASKeywordId >> arity [
	^ 2
]

{ #category : #parsing }
ASKeywordId >> classSelector: anASCompiler args: args [ 
	anASCompiler nextToken.
	args add: (anASCompiler peek parameterFrom: anASCompiler).
	^ anASCompiler peek classSelector: anASCompiler args: args selector: self value
]

{ #category : #parsing }
ASKeywordId >> classSelector: anASCompiler args: args selector: selector [ 
	anASCompiler nextToken.
	args add: (anASCompiler peek parameterFrom: anASCompiler).
	^ anASCompiler peek classSelector: anASCompiler args: args selector: selector,self value
]
