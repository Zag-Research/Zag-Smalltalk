Class {
	#name : 'ASKeywordId',
	#superclass : 'ASIdentifier',
	#category : 'ASTSmalltalk-Tokens',
	#package : 'ASTSmalltalk',
	#tag : 'Tokens'
}

{ #category : 'accessing' }
ASKeywordId >> arity [
	^ 2
]

{ #category : 'parsing' }
ASKeywordId >> classSelector: anASCompiler args: args [ 
	anASCompiler nextToken.
	args add: (anASCompiler peek parameterFrom: anASCompiler).
	^ anASCompiler peek classSelector: anASCompiler args: args selector: self value
]

{ #category : 'parsing' }
ASKeywordId >> classSelector: anASCompiler args: args selector: selector [ 
	anASCompiler nextToken.
	args add: (anASCompiler peek parameterFrom: anASCompiler).
	^ anASCompiler peek classSelector: anASCompiler args: args selector: selector,self value
]

{ #category : 'parsing' }
ASKeywordId >> keywordExpression: anASCompiler target: target [
	^ self keywordExpression: anASCompiler target: target selector: '' args: OrderedCollection new
]

{ #category : 'parsing' }
ASKeywordId >> keywordExpression: anASCompiler target: target selector: selector args: args [
	anASCompiler nextToken.
	args add: (anASCompiler peek binaryExpression: anASCompiler).
	^ anASCompiler peek keywordExpression: anASCompiler target: target selector: selector,value args: args
]
