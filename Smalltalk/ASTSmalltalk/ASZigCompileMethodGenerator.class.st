Class {
	#name : #ASZigCompileMethodGenerator,
	#superclass : #ASZigCodeGenerator,
	#instVars : [
		'numLocals',
		'closureSize',
		'offsets',
		'selector'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASZigCompileMethodGenerator class >> stream: aStream [
 ^ self new stream: aStream
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

{ #category : #generation }
ASZigCompileMethodGenerator >> generateCodeFor: anASMethodNode [

	anASMethodNode zigCompileMethod: self
]

{ #category : #miscellaneous }
ASZigCompileMethodGenerator >> goTo: aLabel [
	"what does a go to look like?"

	self labelRef: aLabel
]

{ #category : #accessing }
ASZigCompileMethodGenerator >> locals: locals arguments: arguments [

	closureSize := 0.
	numLocals := locals size.
	offsets := Dictionary new.
	locals reversed , arguments reversed , #( self ) doWithIndex: [
		:each
		:index | offsets at: each put: index ].

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
		doPush;
		putEmbedded: #pushLiteral;
		nextPutAll: aLiteral asZigLiteral;
		comma
]

{ #category : #API }
ASZigCompileMethodGenerator >> pushLocal: aSelector [

	self
		doPush;
		putEmbedded: #pushLocal;
		print: (offsets at: aSelector);
		comma;
		comment: aSelector
]

{ #category : #API }
ASZigCompileMethodGenerator >> returnTop [

	self putEmbedded: #returnTop
]

{ #category : #accessing }
ASZigCompileMethodGenerator >> selector [

	^	selector
]

{ #category : #accessing }
ASZigCompileMethodGenerator >> selector: aSymbol [
	selector := aSymbol 
]

{ #category : #API }
ASZigCompileMethodGenerator >> send: aSelector [

	self putEmbedded:
		'send' , aSelector numArgs asString;comma;nextPutAll: aSelector;comma 
]

{ #category : #writing }
ASZigCompileMethodGenerator >> writeDefinition [

	context
		writeNewDeclarationFor: self;
		nextPutAll: 'compileMethod(';
		nextPutAll: self name;
		nextPut: $,;
		nextPutAll: numLocals asString;
		nextPut: $,;
		nextPutAll: maxDepth asString;
		nextPutAll: '+';
		nextPutAll: closureSize asString;
		nextPutAll: ',.{';
		nextPutAll: mainStream contents;
		nextPutAll: ' });';
		lf
]

{ #category : #writing }
ASZigCompileMethodGenerator >> writeInitialization [

	context
		nextPutAll: name;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]Object{';
		nextPutAll: self refListString;
		nextPutAll: '},null);';
		lf
]
