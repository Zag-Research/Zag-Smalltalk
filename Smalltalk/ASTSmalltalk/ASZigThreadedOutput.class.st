"
I generate text-based code compiled by the `compileMethod` compile-time Zig function
"
Class {
	#name : #ASZigThreadedOutput,
	#superclass : #ASZigMethodOutput,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #printing }
ASZigThreadedOutput class >> testing [
	^ self new testing
]

{ #category : #API }
ASZigThreadedOutput >> branch: command to: target [
	command = #br ifTrue: [ 
	delayedLabel := target label ] ifFalse: [ self putEmbedded: command. target ifNotNil: [self labelRef: target label ]]
]

{ #category : #API }
ASZigThreadedOutput >> drop [

	self putEmbedded: #drop
]

{ #category : #API }
ASZigThreadedOutput >> endMethod [

	self label: nil
]

{ #category : #'code generation' }
ASZigThreadedOutput >> outputToStream [

	codeGenerator cleanupAndGetPrimitive
		ifNil: [ self putEmbedded: #verifySelector ]
		ifNotNil: [ :primitive | self putPrimitive: primitive ].
	codeGenerator linearizeBasicBlocksDo: [ :bb |
		self label: bb label.
		bb outputTo: self ].
	self endMethod
]

{ #category : #API }
ASZigThreadedOutput >> popLocal: localNumber [

	self
		putEmbedded: #popLocal;
		print: localNumber;
		comma
]

{ #category : #API }
ASZigThreadedOutput >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	self
		putEmbedded: #popLocalData;
		print: stackIndex;
		nextPutAll: ' + (';
		print: blockIndex;
		nextPutAll: ' << 8), // ';
		nextPutAll: name;
		nextPutAll: ' (read-only) copy offset 3 in local 1 (field in BC1)'
]

{ #category : #accessing }
ASZigThreadedOutput >> pushBlock: stackIndex [

	self tab;
	print: stackIndex;lf
]

{ #category : #API }
ASZigThreadedOutput >> pushContext [

	self putEmbedded: #pushContext;
	nextPutAll: '"^"';comma
]

{ #category : #API }
ASZigThreadedOutput >> pushLiteral: aLiteral [

	self
		putEmbedded: #pushLiteral;
		nextPutAll: aLiteral asZigLiteral;
		comma
]

{ #category : #API }
ASZigThreadedOutput >> pushLocal: localNumber [

	self
		putEmbedded: #pushLocal;
		print: localNumber;
		comma
]

{ #category : #API }
ASZigThreadedOutput >> returnTop [

	self
		putEmbedded: #returnTop
]

{ #category : #API }
ASZigThreadedOutput >> send: aSelector useCall: useCall [

	self putEmbedded: (useCall
			 ifTrue: [ 'call' ]
			 ifFalse: [ 'send' ]) , aSelector numArgs asString.
	self
		putSymbol: aSelector
]

{ #category : #API }
ASZigThreadedOutput >> startMethod: primitiveNumber [

	primitiveNumber = 0
		ifTrue: [ self putEmbedded: #verifySelector ]
		ifFalse: [ self putPrimitive: primitiveNumber ]
]

{ #category : #writing }
ASZigThreadedOutput >> writeDefinitionOn: stream [

	context writeNewDeclarationFor: self on: stream.
	stream
		nextPutAll: 'compileMethod(';
		nextPutAll: self name zigName;
		comma;
		print: context numLocals;
		comma;
		print: context maxDepth;
		nextPutAll: '+';
		print: closureSize;
		nextPutAll: ',.{';
		nextPutAll: mainStream contents;
		nextPutAll: ' });';
		lf
]

{ #category : #writing }
ASZigThreadedOutput >> writeInitializationOn: stream [

	stream
		nextPutAll: componentName;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]Object{';
		nextPutAll: self refListString;
		nextPutAll: '},null);';
		lf
]
