"
I compile Smalltalk source from a Tonel file into a class with methods expressed as ASTs
"
Class {
	#name : 'ASSourceCompiler',
	#superclass : 'ASCompiler',
	#instVars : [
		'class',
		'scope',
		'aScanner'
	],
	#category : 'ASTSmalltalk-Compiler',
	#package : 'ASTSmalltalk',
	#tag : 'Compiler'
}

{ #category : 'compilation' }
ASSourceCompiler >> compileMethod: aMethod [
	aScanner := ASScanner new scanCode: aMethod sourceCode readStream.
	^ self compileMethodInClass: (ASClassNode new from: aMethod methodClass)
]

{ #category : 'parsing' }
ASSourceCompiler >> compileMethodInClass: aClass [
	| selector args method savedScope |
	args := OrderedCollection new.
	selector := aScanner peek classSelector: self args: args.
	method := ASMethodNode new selector: selector; arguments: args; class: aClass; yourself.
	args do: [ : arg | method add: arg ].
	savedScope := scope.
	[
		scope := method nestedScopeOf: aClass.
		method body: self methodBody
	] ensure: [
		scope := savedScope
	].
	^ method
]

{ #category : 'parsing' }
ASSourceCompiler >> localDefs [
	aScanner peek defs: self scope: scope.
]

{ #category : 'parsing' }
ASSourceCompiler >> methodBody [
	| statements |
	self localDefs.
	statements := OrderedCollection new.
	self statements: statements.
	(statements notEmpty and: [statements last isReturn]) ifFalse: [ 
		statements add: (ASReturn new unaryExpression: #self asASSelf)
	].
	aScanner peek expectEnd.
	^ statements
]

{ #category : 'scanning' }
ASSourceCompiler >> nextToken [
	^ aScanner next
]

{ #category : 'scanning' }
ASSourceCompiler >> peek [
	^ aScanner peek
]

{ #category : 'parsing' }
ASSourceCompiler >> statements: statements [ 
	| statement |
	[ statement := aScanner peek statement: self ] whileNotNil: [ statements add: statement ].

]
