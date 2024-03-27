"
I generate a call to compileObject for a passed object, and include all the objects referenced by this object.
This is primarily oriented to generating ASTs, but it can handle most objects, including Arrays.
"
Class {
	#name : 'ASZigCompileObjectOutput',
	#superclass : 'Object',
	#instVars : [
		'context',
		'ourObject',
		'alreadyGenerated',
		'textStream',
		'symbols',
		'componentName',
		'label',
		'delayedLabel'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'converting' }
ASZigCompileObjectOutput >> asString [

	^ textStream contents
]

{ #category : 'writing' }
ASZigCompileObjectOutput >> classListString [

	^ ''
]

{ #category : 'printing' }
ASZigCompileObjectOutput >> comma [
	textStream nextPut: $,
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> context: anASFileContext [

	context := anASFileContext.
	anASFileContext addComponent: self
]

{ #category : 'code generation' }
ASZigCompileObjectOutput >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	ourObject zigCompileObject: self
]

{ #category : 'API' }
ASZigCompileObjectOutput >> generateCode: anASNode [

	context for: anASNode generatorIs: self.
	ourObject := anASNode.
	alreadyGenerated := false.
	context processComponents.
	^ self
]

{ #category : 'initialization' }
ASZigCompileObjectOutput >> initialize [

	super initialize.
	textStream := '' writeStream.
	symbols := OrderedCollection new.
	label := 0.
	alreadyGenerated := false
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> label: result [

	delayedLabel = result ifFalse: [
		delayedLabel ifNotNil: [
			self
				putEmbedded: #br;
				labelRef: delayedLabel ] ].
	delayedLabel := nil.
	result ifNotNil: [
		self
			nextPutAll: '":';
			nextPutAll: result;
			nextPut: $";
			comma ].
	^ result
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> labelRef: aString [

	self
		nextPut: $";
		nextPutAll: aString;
		nextPut: $"
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> lookupObject: anObject [

	^ context lookupObject: anObject
]

{ #category : 'output' }
ASZigCompileObjectOutput >> makeNextLabel [

	label := label + 1.
	^ String streamContents: [ :s |
		  s nextPut: $L.
		  label printOn: s ]
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> name [

	^ componentName ifNil: [
		  componentName := context
			                   name: self
			                   prefix: self prefix
			                   main: self selector ]
]

{ #category : 'output' }
ASZigCompileObjectOutput >> nextLabel [

	^ self label: self makeNextLabel
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> nextPut: aCharacter [ 
	^ textStream nextPut: aCharacter
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> nextPutAll: aString [ 
	^ textStream nextPutAll: aString
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> prefix [ ^ 'Obj'
]

{ #category : 'output' }
ASZigCompileObjectOutput >> putClass: aString [

	self
		nextPutAll: 'c.';
		nextPutAll: aString;
		comma
]

{ #category : 'adding' }
ASZigCompileObjectOutput >> putSymbol: aSymbol [

	(context predefinedSymbol: aSymbol)
		ifNotNil: [ :aString | textStream << aString ]
		ifNil: [
			(symbols indexOf: aSymbol ifAbsent: [
				 symbols add: aSymbol.
				 context addSymbol: aSymbol.
				 symbols size ]) printOn: textStream << 'Sym.i_' ]
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> selector [
^ ''
]

{ #category : 'initialization' }
ASZigCompileObjectOutput >> setLiteralString [

	^ String streamContents: [ :s |
		  symbols
			  do: [ :each | s nextPutAll: (context printable: each) ]
			  separatedBy: [ s nextPut: $, ] ]
]

{ #category : 'writing' }
ASZigCompileObjectOutput >> writeDefinitionOn: stream [

	context writeNewDeclarationFor: self on: stream.
	stream
		nextPutAll: 'compileObject(.{';
		nextPutAll: textStream contents;
		nextPutAll: '});';
		lf
]

{ #category : 'writing' }
ASZigCompileObjectOutput >> writeInitializationOn: stream [

	stream
		nextPutAll: componentName;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]ClassIndex{';
		nextPutAll: self classListString;
		nextPutAll: '});';
		lf
]
