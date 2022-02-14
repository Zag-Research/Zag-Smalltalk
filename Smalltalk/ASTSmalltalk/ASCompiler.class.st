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

{ #category : #compilation }
ASCompiler >> compileMethod: aMethod [
	aScanner := ASScanner new scanCode: aMethod sourceCode readStream.
	^ self compileMethodInClass: (ASClassNode new from: aMethod methodClass)
]

{ #category : #parsing }
ASCompiler >> compileMethodInClass: aClass [
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

{ #category : #parsing }
ASCompiler >> localDefs [
	aScanner peek defs: self scope: scope.
]

{ #category : #parsing }
ASCompiler >> methodBody [
	| statements |
	self localDefs.
	statements := OrderedCollection new.
	self statements: statements.
	(statements notEmpty and: [statements last isReturn]) ifFalse: [ 
		statements add: (ASReturnNode new unaryExpression: #self asASSelf)
	].
	aScanner peek expectEnd.
	^ statements
]

{ #category : #scanning }
ASCompiler >> nextToken [
	^ aScanner next
]

{ #category : #scanning }
ASCompiler >> peek [
	^ aScanner peek
]

{ #category : #parsing }
ASCompiler >> statements: statements [ 
	| statement |
	[ statement := aScanner peek statement: self ] whileNotNil: [ statements add: statement ].

]
