"
I generate text-based code compiled by the `compileMethod` compile-time Zig function
"
Class {
	#name : #ASZigThreadedOutput,
	#superclass : #ASZigOutput,
	#instVars : [
		'closureSize',
		'codeGenerator'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #printing }
ASZigThreadedOutput class >> testing [
	^ self new testing
]

{ #category : #accessing }
ASZigThreadedOutput >> closure: stackIndex modifier: size [

	self
		putEmbedded: #closureData;
		print: stackIndex;
		nextPutAll: ' + (';
		print: size;
		nextPutAll: ' << 8), // local:3 size:1 (offset 1 is l1)';
		lf
]

{ #category : #'code generation' }
ASZigThreadedOutput >> codeGenerator [
	^ codeGenerator ifNil: [	codeGenerator := ASCodeGenerator new]
]

{ #category : #API }
ASZigThreadedOutput >> drop [

	self putEmbedded: #drop
]

{ #category : #'code generation' }
ASZigThreadedOutput >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	ourObject zigCompileMethod: self codeGenerator.
	self outputToStream
]

{ #category : #miscellaneous }
ASZigThreadedOutput >> goTo: aLabel [
	"what does a go to look like?"

	self labelRef: aLabel
]

{ #category : #initialization }
ASZigThreadedOutput >> initialize [

	super initialize.
	closureSize := 0
]

{ #category : #'code generation' }
ASZigThreadedOutput >> outputToStream [

	codeGenerator linearizeTo: self
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
ASZigThreadedOutput >> prefix [ ^ 'M'
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

{ #category : #accessing }
ASZigThreadedOutput >> selector [

	^ codeGenerator selector
]

{ #category : #API }
ASZigThreadedOutput >> send: aSelector [

	self
		putEmbedded: 'send' , aSelector numArgs asString;
		nextPutAll: aSelector;
		comma 
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
