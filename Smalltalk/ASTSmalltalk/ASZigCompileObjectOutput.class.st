"
I generate a call to compileObject for a passed object, and include all the objects referenced by this object.
This is primarily oriented to generating ASTs, but it can handle most objects, including Arrays.
"
Class {
	#name : 'ASZigCompileObjectOutput',
	#superclass : 'Object',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'code generation' }
ASZigCompileObjectOutput >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	ourObject zigCompileObject: self
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> lookupObject: anObject [

	^ context lookupObject: anObject
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> prefix [ ^ 'Obj'
]

{ #category : 'accessing' }
ASZigCompileObjectOutput >> selector [
^ ''
]

{ #category : 'writing' }
ASZigCompileObjectOutput >> writeDefinitionOn: stream [

	context writeNewDeclarationFor: self on: stream.
	stream
		nextPutAll: 'compileObject(.{';
		nextPutAll: mainStream contents;
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
