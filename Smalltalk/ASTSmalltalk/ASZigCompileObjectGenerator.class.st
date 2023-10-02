"
I generate a call to compileObject for a passed object, and include all the objects referenced by this object.
This is primarily oriented to generating ASTs, but it can handle most objects, including Arrays.
"
Class {
	#name : #ASZigCompileObjectGenerator,
	#superclass : #ASZigCodeGenerator,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASZigCompileObjectGenerator >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	ourObject zigCompileObject: self
]

{ #category : #accessing }
ASZigCompileObjectGenerator >> prefix [ ^ 'Obj'
]

{ #category : #writing }
ASZigCompileObjectGenerator >> writeDefinition [

	context writeNewDeclarationFor: self;
		nextPutAll: 'compileObject(.{';
		nextPutAll: mainStream contents;
		nextPutAll: ' });';
		lf
]

{ #category : #writing }
ASZigCompileObjectGenerator >> writeInitialization [

	context
		nextPutAll: componentName;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]ClassIndex{';
		nextPutAll: self classListString;
		nextPutAll: '});';
		lf
]
