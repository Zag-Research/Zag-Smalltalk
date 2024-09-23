"
I am an interpreter that validates semantics, with no attempt for efficiency or exact parallels of the Zag runtime
"
Class {
	#name : 'ASemanticInterpreter',
	#superclass : 'Object',
	#instVars : [
		'stack',
		'context',
		'classes',
		'dispatch',
		'lastClass',
		'inlineDepth',
		'pc',
		'executing',
		'basicBlocks'
	],
	#category : 'ASTSmalltalk-Tests-SemanticInterpreter',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'adding' }
ASemanticInterpreter >> addClasses: aCollection [

	aCollection do: [: each |
		lastClass := lastClass + 1.
		each isClass ifTrue: [ classes at: each put: lastClass ] ]
]

{ #category : 'accessing' }
ASemanticInterpreter >> basicBlocks [

	^ basicBlocks
]

{ #category : 'compiling' }
ASemanticInterpreter >> compile: aSymbol for: aClass [

	| cg |
	cg := ASCodeGenerator targetClass: aClass selector: aSymbol.
	cg inline: inlineDepth.
	basicBlocks addAll: cg basicBlocks.
	^ ASemanticOutput new
		  codeGenerator: cg;
		  outputToStream
]

{ #category : 'accessing' }
ASemanticInterpreter >> dispatchFor: aCompileMethod [ 

	| class |
	class := self getClass: aCompileMethod methodClass.
	^ (dispatch at: class ifAbsent: [ ^ nil ]) at: aCompileMethod selector ifAbsent: [ nil ]
]

{ #category : 'adding' }
ASemanticInterpreter >> getClass: aClass [

	^ classes at: aClass ifAbsentPut: [ lastClass := lastClass + 1 ]
]

{ #category : 'adding' }
ASemanticInterpreter >> getMethod: aSymbol for: anObject [

	| classIndex |
	classIndex := self getClass: anObject class.
	^ ((dispatch at: classIndex) ifNil: [
		   dispatch at: classIndex put: IdentityDictionary new ])
		  at: aSymbol
		  ifAbsentPut: [ self compile: aSymbol for: anObject class ]
]

{ #category : 'initialization' }
ASemanticInterpreter >> initialize [

	super initialize.
	inlineDepth := 0.
	self reset
]

{ #category : 'accessing' }
ASemanticInterpreter >> inlineDepth: anInteger [

	inlineDepth := anInteger
]

{ #category : 'interpreting' }
ASemanticInterpreter >> interpret [

	executing := true.
	[ executing ] whileTrue: [ pc doForInterpreter: self ]
]

{ #category : 'interpreting' }
ASemanticInterpreter >> pushLiteral [

	stack addLast: pc next
]

{ #category : 'initialization' }
ASemanticInterpreter >> reset [

	stack := OrderedCollection new.
	context := nil.
	basicBlocks := OrderedCollection new.
	classes := IdentityDictionary new.
	lastClass := 0.
	self addClasses: {
			#ThunkHeap.
			#ThunkReturnLocal.
			#ThunkReturnSmallInteger.
			#ThunkReturnImmediate.
			#ThunkReturnCharacter.
			UndefinedObject.
			True.
			False.
			SmallInteger.
			Symbol.
			Character.
			#ThunkImmediate.
			#ThunkFloat.
			Float.
			Object.
			Array.
			String.
			CompiledMethod.
			Class.
			Context.
			BlockClosure.
			#Method.
			#Dispatch }.
	dispatch := Array new: 50
]

{ #category : 'interpreting' }
ASemanticInterpreter >> returnTOS [

	context ifNil: [
		[ stack size > 1 ] whileTrue: [ stack removeFirst ].
		executing := false.
		^ self ].
	self halt
]

{ #category : 'interpreting' }
ASemanticInterpreter >> send: aSymbol [

	| obj method |
	obj := stack at: stack size - aSymbol numArgs.
	method := self getMethod: aSymbol for: obj.
	pc := method startingPc
]

{ #category : 'initialization' }
ASemanticInterpreter >> send: aSymbol to: anObject [

	self reset.
	stack addLast: anObject.
	self send: aSymbol.
	self interpret.
	^ stack last
]

{ #category : 'initialization' }
ASemanticInterpreter >> send: aSymbol with: arguments to: anObject [

	self reset.
	stack addLast: anObject.
	arguments do: [ :arg | stack addLast: arg ].
	self send: aSymbol.
	self interpret.
	^ stack last
]

{ #category : 'accessing' }
ASemanticInterpreter >> stackDepth [

	^ stack size
]

{ #category : 'as yet unclassified' }
ASemanticInterpreter >> staclDepth [

	^ stack size
]
