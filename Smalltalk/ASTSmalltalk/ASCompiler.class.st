"
I compile Smalltalk source from a Tonel file into a class with methods expressed as ASTs
"
Class {
	#name : #ASCompiler,
	#superclass : #Object,
	#instVars : [
		'class',
		'scope',
		'aScanner'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #parsing }
ASCompiler >> body [
	| token |
	token := aScanner peek.
	token value = $^ ifTrue: [
		aScanner next.
		^ ASReturnNode new expression: self expression
	].
	^ self error: 'Unexpected token: ',token value asString
	
]

{ #category : #compilation }
ASCompiler >> compileMethod: aMethod [
	aScanner := ASScanner new scanCode: aMethod sourceCode readStream.
	^ self compileMethodInClass: (ASClassNode new from: aMethod methodClass)
]

{ #category : #parsing }
ASCompiler >> compileMethodInClass: aClass [
	| token selector args method savedScope |
	token := aScanner next.
	token isIdentifier ifFalse: [ ^ self error: 'Method must start with identifier' ].
	args := OrderedCollection new.
	( token isKeyword ) ifFalse: [
		selector := token value
	] ifTrue: [
		selector := ''.
		[
			selector := selector,token value.
			token := aScanner next.
			token isUnary ifFalse: [ ^ self error: 'Method arguments must be simple identifiers' ].
			args add: token.
			token := aScanner peek.
			token isKeyword
		] whileTrue: [ aScanner next ]
	].
	method := ASMethodNode new selector: selector; arguments: args; class: aClass; yourself.
	savedScope := scope.
	scope := method nestedScopeOf: aClass.
	method body: self body.
	scope := savedScope.
	^ method
]

{ #category : #parsing }
ASCompiler >> expression [
	^ aScanner next
]
