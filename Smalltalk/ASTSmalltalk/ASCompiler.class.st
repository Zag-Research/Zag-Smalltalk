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
	^ Array streamContents: [ :s |
			self localDefs.
		  (self sequenceTo: s then: [ aScanner atEnd ] withReturn: true) ifFalse: [ 
			  s nextPut: (ASReturnNode new expression: #self asASSelf) ].
		  aScanner atEnd ifFalse: [ 
			  ^ self error: 'Unexpected token: ' , aScanner peek asString ]
		]
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
	token isMethodIntro ifFalse: [ ^ self error: 'Method must start with identifier' ].
	args := OrderedCollection new.
	token isKeyword ifFalse: [
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
	[
		scope := method nestedScopeOf: aClass.
		method body: self body
	] ensure: [
		scope := savedScope
	].
	^ method
]

{ #category : #parsing }
ASCompiler >> expression [
	| token |
	token := aScanner next.
]

{ #category : #parsing }
ASCompiler >> localDefs [

]

{ #category : #parsing }
ASCompiler >> sequenceTo: s then: endBlock withReturn: returnAllowed [
	| token |
	[
		[aScanner peek isSeparator] whileTrue: [aScanner next].
		token := aScanner peek.
		endBlock value or: [token isReturn or: [s nextPut: (self statementThen: endBlock). true]]
	] whileFalse: [
		aScanner next
	].
	token isReturn ifTrue: [
		returnAllowed ifFalse: [ self error: 'unexpected return'].
		aScanner next.
		s nextPut: (ASReturnNode new expression: self expression).
		[ aScanner peek isSeparator ] whileTrue: [ aScanner next ].
		^ true
	].
	[ aScanner peek isSeparator ] whileTrue: [ aScanner next ].
	^ false
]

{ #category : #parsing }
ASCompiler >> statementThen: aBlock [
	| result |
	result := self expression.
	(aScanner peek isSeparator or: [ aBlock value ]) ifTrue: [self error: 'missing separator'].
	[ aScanner peek isSeparator ] whileTrue: [ aScanner next ].
	^ result
]
