"
I am an interpreter that validates semantics, with no attempt for efficiency or exact parallels of the Zag runtime
"
Class {
	#name : 'ASemanticInterpreter',
	#superclass : 'Object',
	#instVars : [
		'context',
		'classes',
		'dispatch',
		'lastClass',
		'inlineDepth',
		'pc',
		'executing',
		'basicBlocks',
		'stackArray',
		'stackPointer'
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

{ #category : 'callgraph' }
ASemanticInterpreter >> asDotString [

	^ String streamContents: [ :s |
		  s << 'digraph structs {
  node [shape=record];
'.
		  basicBlocks do: [ :bb | bb dotStringOn: s ].
		  s << '}' ]
]

{ #category : 'accessing' }
ASemanticInterpreter >> basicBlocks [

	^ basicBlocks
]

{ #category : 'accessing' }
ASemanticInterpreter >> codeSize [

	^ basicBlocks size
]

{ #category : 'compiling' }
ASemanticInterpreter >> compile: aSymbol for: aClass [

	| cg |
	cg := ASCodeGenerator targetClass: aClass selector: aSymbol.
	cg inline: inlineDepth.
	cg finalAnalysis.
	basicBlocks addAll: cg basicBlocks.
	^ cg basicBlocks first
]

{ #category : 'menu morph' }
ASemanticInterpreter >> createContext [

	context := ASIContext new sendingContext: context
]

{ #category : 'private - testing' }
ASemanticInterpreter >> dispatchFor: aCompileMethod [ 

	| class |
	class := self getClass: aCompileMethod methodClass.
	^ (dispatch at: class ifAbsent: [ ^ nil ]) at: aCompileMethod selector ifAbsent: [ nil ]
]

{ #category : 'handling' }
ASemanticInterpreter >> endInterpretation [

	executing := false
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
	self stackSize: 500.
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

{ #category : 'accessing' }
ASemanticInterpreter >> pop [

	stackPointer := stackPointer + 1.
	^ stackArray at: stackPointer - 1
]

{ #category : 'accessing' }
ASemanticInterpreter >> push: anObject [

	stackPointer := stackPointer - 1.
	^ stackArray at: stackPointer put: anObject
]

{ #category : 'initialization' }
ASemanticInterpreter >> reset [

	stackPointer := stackArray size + 1.
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

{ #category : 'handling' }
ASemanticInterpreter >> return: anObject selfOffset: selfOffset [

	stackPointer := stackPointer + selfOffset.
	stackArray at: stackPointer put: anObject.
	pc := context returnPC
]

{ #category : 'accessing' }
ASemanticInterpreter >> returnTo: aBasicBlock [

	^ context returnPC: (ASIProgramCounter new forBasicBlock: aBasicBlock)
]

{ #category : 'interpreting' }
ASemanticInterpreter >> send: aSymbol [

	| obj method |
	obj := self stackAt: aSymbol numArgs.
	method := self getMethod: aSymbol for: obj.
	pc := ASIProgramCounter new forBasicBlock: method
]

{ #category : 'private - testing' }
ASemanticInterpreter >> send: aSymbol to: anObject [

	^ self send: aSymbol with: #() to: anObject
]

{ #category : 'private - testing' }
ASemanticInterpreter >> send: aSymbol with: arguments to: anObject [

	self reset.
	self push: anObject.
	arguments do: [ :arg | self push: arg ].
	self send: aSymbol.
	context := ASIContext new returnPC: ASINullProgramCounter new.
	self interpret.
	^ self top
]

{ #category : 'accessing' }
ASemanticInterpreter >> stackAt: anInteger [

	^ stackArray at: stackPointer + anInteger
]

{ #category : 'private - testing' }
ASemanticInterpreter >> stackDepth [

	^ stackArray size + 1 - stackPointer
]

{ #category : 'initialization' }
ASemanticInterpreter >> stackSize: stackSize [

	stackArray := Array new: stackSize.
	stackPointer := stackSize + 1
]

{ #category : 'accessing' }
ASemanticInterpreter >> top [

	^ stackArray at: stackPointer
]
