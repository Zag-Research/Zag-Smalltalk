"
I generate a call to compileObject for a passed object, and include all the objects referenced by this object.
This is primarily oriented to generating ASTs, but it can handle most objects, including Arrays.
"
Class {
	#name : #ASZigCompileObjectGenerator,
	#superclass : #ASZigCodeGenerator,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #generation }
ASZigCompileObjectGenerator >> generateCodeFor: anASMethodNode [
	anASMethodNode zigCompileObject: self
]

{ #category : #writing }
ASZigCompileObjectGenerator >> writeDefinition [

	name := context writeNewDeclaration.
	context nextPutAll: 'compileObject(.{';
	nextPutAll: mainStream contents;
	nextPutAll: ' });';
	lf
]

{ #category : #writing }
ASZigCompileObjectGenerator >> writeInitialization [

	context
		nextPutAll: name;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]ClassIndex{';
		nextPutAll: self classListString;
		nextPutAll: '});';
		lf
]
