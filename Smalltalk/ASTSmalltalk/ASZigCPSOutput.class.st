"
I generate text-based as a sequence of Continuation-Passing-Style Zig function
"
Class {
	#name : #ASZigCPSOutput,
	#superclass : #ASZigMethodOutput,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #printing }
ASZigCPSOutput class >> testing [
	^ self new testing
]

{ #category : #API }
ASZigCPSOutput >> drop [

	self putEmbedded: #drop
]

{ #category : #API }
ASZigCPSOutput >> goTo: aLabel [
	"what does a go to look like?"

	self labelRef: aLabel
]

{ #category : #initialization }
ASZigCPSOutput >> initialize [

	super initialize.
]

{ #category : #'code generation' }
ASZigCPSOutput >> outputToStream [
	^ self shouldBeImplemented
]

{ #category : #API }
ASZigCPSOutput >> popLocal: localNumber [

	self
		putEmbedded: #popLocal;
		print: localNumber;
		comma
]

{ #category : #API }
ASZigCPSOutput >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	self
		putEmbedded: #popLocalData;
		print: stackIndex;
		nextPutAll: ' + (';
		print: blockIndex;
		nextPutAll: ' << 8), // ';
		nextPutAll: name;
		nextPutAll: ' (read-only) copy offset 3 in local 1 (field in BC1)'
]

{ #category : #API }
ASZigCPSOutput >> pushBlock: stackIndex [

	self tab;
	print: stackIndex;lf
]

{ #category : #API }
ASZigCPSOutput >> pushContext [

	self putEmbedded: #pushContext;
	nextPutAll: '"^"';comma
]

{ #category : #API }
ASZigCPSOutput >> pushLiteral: aLiteral [

	self
		putEmbedded: #pushLiteral;
		nextPutAll: aLiteral asZigLiteral;
		comma
]

{ #category : #API }
ASZigCPSOutput >> pushLocal: localNumber [

	self
		putEmbedded: #pushLocal;
		print: localNumber;
		comma
]

{ #category : #API }
ASZigCPSOutput >> returnTop [

	self
		putEmbedded: #returnTop
]

{ #category : #API }
ASZigCPSOutput >> send: aSelector useCall: useCall [

	self putEmbedded: (useCall
			 ifTrue: [ 'call' ]
			 ifFalse: [ 'send' ]) , aSelector numArgs asString.
	self
		nextPutAll: aSelector;
		comma
]

{ #category : #API }
ASZigCPSOutput >> startMethod: primitiveNumber [

	primitiveNumber = 0
		ifTrue: [ self putEmbedded: #verifySelector ]
		ifFalse: [ self putPrimitive: primitiveNumber ]
]

{ #category : #writing }
ASZigCPSOutput >> writeDefinitionOn: stream [

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
ASZigCPSOutput >> writeInitializationOn: stream [

	stream
		nextPutAll: componentName;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]Object{';
		nextPutAll: self refListString;
		nextPutAll: '},null);';
		lf
]
