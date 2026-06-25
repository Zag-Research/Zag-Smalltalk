"
I compile Smalltalk source from a Tonel file into a class with methods expressed as ASTs
"
Class {
	#name : 'ZagSmalltalk',
	#superclass : 'ASCompiler',
	#instVars : [
		'class',
		'aScanner',
		'stream',
		'indent'
	],
	#category : 'Zag-Compiler-Core',
	#package : 'Zag-Compiler',
	#tag : 'Core'
}

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

{ #category : 'accessing' }
ZagSmalltalk >> indent [

	indent timesRepeat: [ stream tab ]
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
	(statements notEmpty and: [ statements last isReturn ]) ifFalse: [
		statements add: (ASReturn new unaryExpression: (ASSelfToken with: #self) ) ].
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

{ #category : 'accessing' }
ZagSmalltalk >> visit: anAST [

	indent := 0.
	^ anAST accept: self
]

{ #category : 'visiting' }
ZagSmalltalk >> visit: anObject withSurroundingBrackets: bracketsEnabled [
	"Visits anObject.  If bracketsEnabled, print brackets around it."
	bracketsEnabled ifTrue: [ stream nextPut: $( ].
	anObject accept: self.
	bracketsEnabled ifTrue: [ stream nextPut: $) ].
]

{ #category : 'visiting' }
ZagSmalltalk >> visitArray: anArray [
	| isFirst |
	isFirst := true.
	stream nextPutAll: '{ '.
	anArray do: [ :each |
		isFirst
			ifTrue: [ isFirst := false ]
			ifFalse: [ stream nextPutAll: '. ' ].
		each accept: self.
	].
	stream nextPutAll: ' }'.
]

{ #category : 'visiting' }
ZagSmalltalk >> visitAssign: anAssign [
	anAssign variable accept: self.
	stream nextPutAll: ' := '.
	anAssign expression accept: self.
]

{ #category : 'visiting' }
ZagSmalltalk >> visitBlock: aBlock [
	^self visitCodeBlock: aBlock
]

{ #category : 'visiting' }
ZagSmalltalk >> visitCascade: aCascade [
	| isFirst |
	self visitSendTarget: aCascade.
	isFirst := true.
	aCascade sends do: [ :send |
		isFirst
			ifTrue: [ isFirst := false ]
			ifFalse: [ stream nextPut: $; ].
		self visitSendArguments: send.
	]
]

{ #category : 'visiting' }
ZagSmalltalk >> visitCodeBlock: aCodeBlock [

	| first |
	stream nextPutAll: aCodeBlock postComment.
	first := true.
	aCodeBlock locals do: [ :local |
		first
			ifTrue: [
				self indent.
				stream nextPutAll: '| '.
				first := false ]
			ifFalse: [ stream nextPut: $  ].
		local accept: self ].
	first ifFalse: [
			stream nextPutAll: ' |
' ].
	first := true.
	aCodeBlock body do: [ :statement |
		first
			ifTrue: [ first := false ]
			ifFalse: [
						stream nextPutAll: '.
' ].
		self indent.
		statement accept: self ]
]

{ #category : 'visiting' }
ZagSmalltalk >> visitId: anId [
	self shouldBeImplemented.
]

{ #category : 'visiting' }
ZagSmalltalk >> visitLiteral: aLiteral [

	stream print: aLiteral literal
]

{ #category : 'visiting' }
ZagSmalltalk >> visitMethod: aMethodNode [

	| doSpace |
	stream nextPutAll: aMethodNode preComment.
	aMethodNode arguments isEmpty
		ifTrue: [ stream nextPutAll: aMethodNode selector ]
		ifFalse: [
				doSpace := false.
				(aMethodNode selector last = $: ifTrue: [($: split: aMethodNode selector) allButLast] ifFalse: [ {aMethodNode selector}])
					with: aMethodNode arguments
					do: [ :name :arg |
							doSpace
								ifTrue: [ stream space ]
								ifFalse: [ doSpace := true ].
							stream
								nextPutAll: name;
								nextPutAll: ': '.
							arg accept: self ] ].
	indent := 1.
	self visitCodeBlock: aMethodNode
]

{ #category : 'visiting' }
ZagSmalltalk >> visitParameter: anASParameter [

	stream nextPutAll: anASParameter id
]

{ #category : 'visiting' }
ZagSmalltalk >> visitRef: aRef [
	stream nextPutAll: aRef variable
]

{ #category : 'visiting' }
ZagSmalltalk >> visitReturn: anASReturn [

	stream nextPutAll: '^ '.
	anASReturn expression accept: self
]

{ #category : 'visiting' }
ZagSmalltalk >> visitSend: aSend [
	self visitSendTarget: aSend; visitSendArguments: aSend.
]

{ #category : 'visiting' }
ZagSmalltalk >> visitSendArguments: aSend [
	"Accepts an ASSend and adds its selector/arguments to my stream.  Does NOT print its target."
	aSend 
		ifUnary: [ self visitSendArgumentsUnary: aSend ]
		ifBinary: [ self visitSendArgumentsBinary: aSend ]
		ifKeyword: [ self visitSendArgumentsKeyword: aSend ].
]

{ #category : 'visiting' }
ZagSmalltalk >> visitSendArgumentsBinary: aSend [
	"visitSendArguments:, where the ASSend is unary."
	stream nextPut: Character space; nextPutAll: aSend selector; nextPut: Character space.
	(aSend argNeedsBrackets: aSend args first) ifTrue: [ stream nextPut: $( ].
	aSend args first accept: self.
	(aSend argNeedsBrackets: aSend args first) ifTrue: [ stream nextPut: $) ].
]

{ #category : 'visiting' }
ZagSmalltalk >> visitSendArgumentsKeyword: aSend [
	"Accepts an ASSend and adds its selector/arguments to my stream.  Does NOT print its target."
	| sendParts |
	sendParts := (aSend selector asString splitOn: ':') allButLast.
	sendParts size = aSend args size ifFalse: [ self error: 'ASSend has wrong number of arguments.' ].
	1 to: sendParts size do: [ :i |
		stream nextPut: Character space; nextPutAll: (sendParts at: i); nextPutAll: ': '.
		(aSend argNeedsBrackets: (aSend args at: i)) ifTrue: [ stream nextPut: $( ].
		(aSend args at: i) accept: self.
		(aSend argNeedsBrackets: (aSend args at: i)) ifTrue: [ stream nextPut: $) ].
	]
]

{ #category : 'visiting' }
ZagSmalltalk >> visitSendArgumentsUnary: aSend [
	"visitSendArguments:, where the ASSend is unary."
	stream nextPut: Character space; nextPutAll: aSend selector.
]

{ #category : 'visiting' }
ZagSmalltalk >> visitSendTarget: aSend [
	"Prints the target of an ASSend or ASCascade."
	aSend targetNeedsBrackets ifTrue: [ stream nextPut: $( ].
	aSend target accept: self.
	aSend targetNeedsBrackets ifTrue: [ stream nextPut: $) ].
]
