"
I generate text-based code compiled by the `compileMethod` compile-time Zig function
"
Class {
	#name : #ASZigCompileMethodGenerator,
	#superclass : #ASZigCodeGenerator,
	#instVars : [
		'closureSize'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #printing }
ASZigCompileMethodGenerator class >> testing [
	^ self new testing
]

{ #category : #accessing }
ASZigCompileMethodGenerator >> closure: stackIndex modifier: size [

	self
		putEmbedded: #closureData;
		print: stackIndex;
		nextPutAll: ' + (';
		print: size;
		nextPutAll: ' << 8), // local:3 size:1 (offset 1 is l1)';
		lf
]

{ #category : #API }
ASZigCompileMethodGenerator >> drop [

	self putEmbedded: #drop
]

{ #category : #'code generation' }
ASZigCompileMethodGenerator >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	ourObject zigCompileMethod: self
]

{ #category : #miscellaneous }
ASZigCompileMethodGenerator >> goTo: aLabel [
	"what does a go to look like?"

	self labelRef: aLabel
]

{ #category : #initialization }
ASZigCompileMethodGenerator >> initialize [

	super initialize.
	closureSize := 0
]

{ #category : #API }
ASZigCompileMethodGenerator >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

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
ASZigCompileMethodGenerator >> prefix [ ^ 'M'
]

{ #category : #API }
ASZigCompileMethodGenerator >> primitiveOrVerify: primitiveNumber [

	primitiveNumber = 0
		ifTrue: [ self putEmbedded: #verifySelector ]
		ifFalse: [ self putPrimitive: primitiveNumber ]
]

{ #category : #accessing }
ASZigCompileMethodGenerator >> pushBlock: stackIndex [

	self tab;
	print: stackIndex;lf
]

{ #category : #API }
ASZigCompileMethodGenerator >> pushContext [

	self putEmbedded: #pushContext;
	nextPutAll: '"^"';comma
]

{ #category : #API }
ASZigCompileMethodGenerator >> pushLiteral: aLiteral [

	self
		doPush: aLiteral;
		putEmbedded: #pushLiteral;
		nextPutAll: aLiteral asZigLiteral;
		comma
]

{ #category : #API }
ASZigCompileMethodGenerator >> pushLocal: aSymbol [

	self
		doPush: (self local: aSymbol);
		putEmbedded: #pushLocal;
		print: (offsets at: aSymbol);
		comma;
		comment: aSymbol
]

{ #category : #API }
ASZigCompileMethodGenerator >> returnTop [

	self putEmbedded: #returnTop
]

{ #category : #API }
ASZigCompileMethodGenerator >> send: aSelector [

	self
		putEmbedded: 'send' , aSelector numArgs asString;
		nextPutAll: aSelector;
		comma 
]

{ #category : #writing }
ASZigCompileMethodGenerator >> writeDefinitionOn: stream [

	context
		writeNewDeclarationFor: self on: stream.
		stream
		nextPutAll: 'compileMethod(';
		nextPutAll: self name zigName;
		comma;
		print: numLocals;
		comma;
		print: maxDepth;
		nextPutAll: '+';
		print: closureSize;
		nextPutAll: ',.{';
		nextPutAll: mainStream contents;
		nextPutAll: ' });';
		lf
]

{ #category : #writing }
ASZigCompileMethodGenerator >> writeInitializationOn: stream [

	stream
		nextPutAll: componentName;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]Object{';
		nextPutAll: self refListString;
		nextPutAll: '},null);';
		lf
]
