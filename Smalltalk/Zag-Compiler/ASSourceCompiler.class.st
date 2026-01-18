"
I compile Smalltalk source from a Tonel file into a class with methods expressed as ASTs
"
Class {
	#name : 'ASSourceCompiler',
	#superclass : 'ASCompiler',
	#instVars : [
		'class',
		'aScanner'
	],
	#category : 'Zag-Compiler-Core',
	#package : 'Zag-Compiler',
	#tag : 'Core'
}

{ #category : 'parsing' }
ASSourceCompiler >> compileInClass: aClass [

	| selector args method savedScope |
	args := OrderedCollection new.
	selector := aScanner peek classSelector: self args: args.
	method := ASMethodNode new
		          selector: selector;
		          arguments: args;
		          classContext: aClass;
		          yourself.
	args do: [ :arg | method addToScope: arg ].
	savedScope := currentScope.
	[
		currentScope := method nestedScopeOf: aClass.
		method body: self methodBody ] ensure: [ currentScope := savedScope ].
	^ method
]

{ #category : 'compiling' }
ASSourceCompiler >> compileMethod: aMethod [

	^ self
		  compileMethod: aMethod sourceCode
		  inClass: (ASClassNode new from: aMethod methodClass)
]

{ #category : 'compilation' }
ASSourceCompiler >> compileMethod: sourceCode inClass: aClass [

	aScanner := ASScanner new scanCode: sourceCode readStream.
	^ self compileInClass: aClass
]

{ #category : 'parsing' }
ASSourceCompiler >> localDefs [

	aScanner peek defs: self scope: currentScope
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
