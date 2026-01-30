"
I compile Smalltalk source from a Tonel file into a class with methods expressed as ASTs
"
Class {
	#name : 'ZagSmalltalk',
	#superclass : 'ASCompiler',
	#instVars : [
		'class',
		'aScanner',
		'stream'
	],
	#category : 'Zag-Compiler-Core',
	#package : 'Zag-Compiler',
	#tag : 'Core'
}

{ #category : 'visiting' }
ZagSmalltalk >> acceptLiteral: anObject [

	stream print: anObject
]

{ #category : 'visiting' }
ZagSmalltalk >> acceptMethod: aMethodNode [

	self shouldBeImplemented.
]

{ #category : 'parsing' }
ZagSmalltalk >> compileInClass: aClass [

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
ZagSmalltalk >> compileMethod: aMethod [

	^ self
		  compileMethod: aMethod sourceCode
		  inClass: (ASClassNode new from: aMethod methodClass)
]

{ #category : 'compilation' }
ZagSmalltalk >> compileMethod: sourceCode inClass: aClass [

	aScanner := ZagSmalltalkScanner scan: sourceCode.
	^ self compileInClass: aClass
]

{ #category : 'parsing' }
ZagSmalltalk >> localDefs [

	aScanner peek defs: self scope: currentScope
]

{ #category : 'parsing' }
ZagSmalltalk >> methodBody [
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
ZagSmalltalk >> nextToken [
	^ aScanner next
]

{ #category : 'scanning' }
ZagSmalltalk >> peek [
	^ aScanner peek
]

{ #category : 'parsing' }
ZagSmalltalk >> statements: statements [ 
	| statement |
	[ statement := aScanner peek statement: self ] whileNotNil: [ statements add: statement ].

]

{ #category : 'accessing' }
ZagSmalltalk >> stream: aWriteStream [

	stream := aWriteStream
]
