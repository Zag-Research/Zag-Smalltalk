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
		'inlineDepth'
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

{ #category : 'compiling' }
ASemanticInterpreter >> compile: aString for: anInteger [

	| cg |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #fibonacci.
	cg
		inline: inlineDepth;
		finalAnalysis.
	^ ASemanticOutput codeGenerator: cg
]

{ #category : 'as yet unclassified' }
ASemanticInterpreter >> getMethod: aSymbol for: anObject [

	| index |
	index := classes at: anObject class ifAbsentPut: [ lastClass := lastClass + 1 ].
	self interpret: (((dispatch at: index) ifNil: [ dispatch at: index put: IdentityDictionary new ]) at: aSymbol ifAbsentPut: [ self compile: aSymbol for: anObject ])
]

{ #category : 'initialization' }
ASemanticInterpreter >> initialize [

	super initialize.
	stack := OrderedCollection new.
	classes := IdentityDictionary new.
	lastClass := 0.
	inlineDepth := 0.
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

{ #category : 'accessing' }
ASemanticInterpreter >> inlineDepth: anInteger [

	inlineDepth := anInteger
]

{ #category : 'instructions' }
ASemanticInterpreter >> send: aSymbol [

	| obj method |
	obj := stack at: stack size - aSymbol numArgs.
	method := self getMethod: aSymbol for: obj.
	self shouldBeImplemented.
]

{ #category : 'as yet unclassified' }
ASemanticInterpreter >> send: aSymbol to: anObject [

	stack := OrderedCollection new.
	context := nil.
	stack addLast: anObject.
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
	dispatch := Array new: 50.
	self send: aSymbol
]
