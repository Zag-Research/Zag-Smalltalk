Class {
	#name : #ASZigCompileObjectGenerator,
	#superclass : #Object,
	#instVars : [
		'literals',
		'context',
		'mainStream',
		'symbols',
		'label',
		'name'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #converting }
ASZigCompileObjectGenerator >> asCompileObject [
	| result |
	"basic template for a compile method"
	result := OrderedCollection new.
	
	result add: 'test "method object" {'.

	result add: '//', contextClass name.
	result add: 'const c = ClassIndex;'. "porobably need to store c as a prefix, or variable"
	result add: 'var o = compileObject(.{'.
	
	methods do: [ :m | result addAll: m asZagCompileObject].
	
	result add:  '});'.
	result add: 'o.setLiterals(&[_]Object{},&[_]ClassIndex{});'.
   result add: ' const method = o.asObject();'.
   result add: ' _ = method;'.
	result add: '}'.
	
	^result
]

{ #category : #initialization }
ASZigCompileObjectGenerator >> asString [
	^ mainStream contents
]

{ #category : #writing }
ASZigCompileObjectGenerator >> classListString [
		^ ''
]

{ #category : #'accessing - keys - symbols' }
ASZigCompileObjectGenerator >> comma [
	self nextPut: $,
]

{ #category : #accessing }
ASZigCompileObjectGenerator >> context: anASFileContext [

	context := anASFileContext.
	anASFileContext add: self
]

{ #category : #generation }
ASZigCompileObjectGenerator >> generateCodeFor: anASMethodNode [
	anASMethodNode asZig: self
]

{ #category : #initialization }
ASZigCompileObjectGenerator >> initialize [

	super initialize.
	mainStream := '' writeStream.
	symbols := OrderedCollection new.
	label := 0
]

{ #category : #accessing }
ASZigCompileObjectGenerator >> labelRef: aString [

	self
		nextPut: $";
		nextPutAll: aString;
		nextPut: $"
]

{ #category : #accessing }
ASZigCompileObjectGenerator >> literals: anASLiterals [ 
	literals := anASLiterals 
]

{ #category : #output }
ASZigCompileObjectGenerator >> nextLabel [

	| result |
	label := label + 1.
	result := String streamContents: [ :s |
		          s nextPut: $L.
		          label printOn: s ].
	self
		nextPutAll: '":';
		nextPutAll: result;
		nextPut: $";
		comma.
	^ result
]

{ #category : #accessing }
ASZigCompileObjectGenerator >> nextPut: aCharacter [ 
	mainStream nextPut: aCharacter
]

{ #category : #accessing }
ASZigCompileObjectGenerator >> nextPutAll: aString [ 
	mainStream nextPutAll: aString
]

{ #category : #output }
ASZigCompileObjectGenerator >> putClass: aString [ 
self nextPutAll: 'c.';nextPutAll: aString;comma
]

{ #category : #output }
ASZigCompileObjectGenerator >> putSymbol: aSymbol [

	(context predefinedSymbol: aSymbol)
		ifNotNil: [ :aString | mainStream << aString ]
		ifNil: [
			(symbols indexOf: aSymbol ifAbsent: [
				 symbols add: aSymbol.
				 context addSymbol: aSymbol.
				 symbols size ]) printOn: mainStream << 'Sym.i_' ]
]

{ #category : #initialization }
ASZigCompileObjectGenerator >> setLiteralString [

	^ String streamContents: [ :s | symbols do: [:each| s << (context printable: each) ] separatedBy: [s << ',']]
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
