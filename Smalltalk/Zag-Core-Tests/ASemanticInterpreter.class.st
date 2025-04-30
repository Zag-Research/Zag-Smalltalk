"
I am an interpreter that validates semantics, with no attempt for efficiency or exact parallels of the Zag runtime
"
Class {
	#name : 'ASemanticInterpreter',
	#superclass : 'Object',
	#instVars : [
		'stackArray',
		'stackPointer',
		'context',
		'pc',
		'primitiveSuccessStackPointer',
		'classes',
		'dispatch',
		'lastClass',
		'inlineDepth',
		'executing',
		'basicBlocks',
		'blockValueSymbols'
	],
	#pools : [
		'ZagConstants'
	],
	#category : 'Zag-Core-Tests-SemanticInterpreter',
	#package : 'Zag-Core-Tests',
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

{ #category : 'primitive execution' }
ASemanticInterpreter >> binary: aBlock [

	primitiveSuccessStackPointer := stackPointer + 1.
	^ (aBlock value: (stackArray at: stackPointer + 1) value: (stackArray at: stackPointer))
		  processPrimitiveResult: self
]

{ #category : 'interpreting' }
ASemanticInterpreter >> branch: aBasicBlock [
	pc := ASIProgramCounter new forBasicBlock: aBasicBlock
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

{ #category : 'accessing' }
ASemanticInterpreter >> contextAt: anInteger [

	^ context at: anInteger
]

{ #category : 'accessing' }
ASemanticInterpreter >> copiledUnitAt: anInteger [ 
	self shouldBeImplemented.
]

{ #category : 'interpreting' }
ASemanticInterpreter >> createContext: aCompiledMethodOrBlock [

	| contextPtr size locals |
	locals := aCompiledMethodOrBlock locals.
	contextPtr := stackPointer - locals - 1.
	size := aCompiledMethodOrBlock arguments + locals + 1.
	self halt.
	context := ASIContext new
		           array: stackArray offset: contextPtr size: size;
		           sendingContext: context.
	stackPointer := contextPtr.
	stackArray at: contextPtr put: context.
	contextPtr + 1 to: contextPtr + locals do: [ :idx |
	stackArray at: idx put: nil ]
]

{ #category : 'interpreting' }
ASemanticInterpreter >> createContext: locals extra: extra [

	| contextPtr size |
	contextPtr := stackPointer - locals - 1.
	size := BaseAddress - (contextPtr + 1).
	context := ASIContext new
		           array: stackArray
		           offset: contextPtr
		           size: size;
		           sendingContext: context;
					  extra: extra.
	stackPointer := contextPtr - extra.
	stackArray at: contextPtr put: context.
	stackPointer to: contextPtr - 1 do: [ :idx |
		stackArray at: idx put: nil ].
	contextPtr + 1 to: contextPtr + locals do: [ :idx |
		stackArray at: idx put: nil ]
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

	self halt.
	^ anObject getMethodFor: aSymbol else: [ :aClass |
		  | classIndex |
		  classIndex := self getClass: aClass.
		  ((dispatch at: classIndex) ifNil: [
			   dispatch at: classIndex put: IdentityDictionary new ])
			  at: aSymbol
			  ifAbsentPut: [ self compile: aSymbol for: aClass ]]
]

{ #category : 'initialization' }
ASemanticInterpreter >> initialize [

	super initialize.
	inlineDepth := 0.
	self stackSize: 10.
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

{ #category : 'primitive execution' }
ASemanticInterpreter >> primitiveSuccess: anObject [

	stackPointer := primitiveSuccessStackPointer.
	stackArray at: stackPointer put: anObject.
	pc := context returnPC
]

{ #category : 'accessing' }
ASemanticInterpreter >> push: anObject [

	stackPointer := stackPointer - 1.
	^ stackArray at: stackPointer put: anObject
]

{ #category : 'API' }
ASemanticInterpreter >> pushBlock: aBasicBlock [ 

	self push: aBasicBlock
]

{ #category : 'initialization' }
ASemanticInterpreter >> reset [

	stackPointer := stackArray size + 1.
	context := nil.
	basicBlocks := OrderedCollection new.
	classes := IdentityDictionary new.
	lastClass := 0.
	blockValueSymbols := { 
		#value.
		#value:.
		#value:value:.
		#value:value:value:.
		#value:value:value:value:
	 }.
	self addClasses: {
			#ThunkHeap.
			#ThunkReturnLocal.
			#ThunkReturnSmallInteger.
			#ThunkReturnImmediate.
			#ThunkReturnCharacter.
			#ThunkImmediate.
			#ThunkFloat.
			False.
			True.
			SmallInteger.
			Symbol.
			Character.
			#ShortString.
			nil . nil . nil . nil . nil . nil . nil . nil . nil . nil . nil . nil . nil . nil .
			UndefinedObject.
			Float.
			ProtoObject.
			Object.
			BlockClosure.
			#BlockClosureValue.
			Context.
			Array.
			String.
			#Utf8String.
			DoubleWordArray.
			Process.
			Class.
			CompiledMethod.
			#Dispatch }.
	dispatch := Array new: 50
]

{ #category : 'as yet unclassified' }
ASemanticInterpreter >> restructure: anInteger stack: selfPointer [

	| old sources size |
	sources := anInteger bitShift: -8.
	size := 0.
	[ sources isZero ] whileFalse: [
		size := size max: (sources bitAnd: 16rff).
		sources := sources bitShift: -8 ].
	old := (Array new: size)
		       replaceFrom: 1
		       to: size
		       with: stackArray
		       startingAt: stackPointer.
	stackPointer := selfPointer + 1.
	sources := anInteger bitShift: -8.
	[ sources isZero ] whileFalse: [
		self push: (old at: (sources bitAnd: 16rff)).
		sources := sources bitShift: -8 ]
]

{ #category : 'as yet unclassified' }
ASemanticInterpreter >> restructureContext: anInteger [

	| offset |
	offset := anInteger bitAnd: 16rff.
	context isOnStack
		ifTrue: [
			self
				restructure: anInteger
				stack: context offset + (anInteger bitAnd: 16rff) ]
		ifFalse: [
			stackPointer := stackArray size + 1 - (context sizeFrom: offset).
			stackArray
				replaceFrom: stackPointer
				to: stackArray size
				with: context array
				startingAt: offset ].
	pc := context returnPC.
	context := context sendingContext
]

{ #category : 'as yet unclassified' }
ASemanticInterpreter >> restructureStack: anInteger [

	^ self restructure: anInteger stack: stackPointer + (anInteger bitAnd: 16rff) - 1
]

{ #category : 'handling' }
ASemanticInterpreter >> return: anObject selfContext: selfOffset [ 
	stackPointer := stackPointer + selfOffset + 1.
	stackArray at: stackPointer put: anObject.
	pc := context sendingContext returnPC 
]

{ #category : 'handling' }
ASemanticInterpreter >> return: anObject selfOffset: selfOffset [

	stackPointer := stackPointer + selfOffset.
	stackArray at: stackPointer put: anObject.
	pc := context returnPC
]

{ #category : 'as yet unclassified' }
ASemanticInterpreter >> returnNonLocal: anObject [ 

	| blockDefiningContext |
	stackPointer := stackArray size - (stackArray indexOf: blockDefiningContext).
	stackArray at: stackPointer put: anObject.
	pc := blockDefiningContext returnPC
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

	self halt.
	self reset.
	self push: anObject.
	arguments do: [ :arg | self push: arg ].
	self send: aSymbol.
	context := ASIContext new returnPC: ASINullProgramCounter new; selfObject: anObject.
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

{ #category : 'primitive execution' }
ASemanticInterpreter >> ternary: aBlock [

	primitiveSuccessStackPointer := stackPointer + 2.
	^ (aBlock
		   value: (stackArray at: primitiveSuccessStackPointer)
			value: (stackArray at: stackPointer + 1)
		   value: (stackArray at: stackPointer)) processPrimitiveResult:
		  self
]

{ #category : 'accessing' }
ASemanticInterpreter >> top [

	^ stackArray at: stackPointer
]

{ #category : 'accessing' }
ASemanticInterpreter >> top: size [

	^ (Array new: size)
		                replaceFrom: 1
		                to: size
		                with: stackArray 
		                startingAt: stackPointer
]

{ #category : 'primitive execution' }
ASemanticInterpreter >> unary: aBlock [

	primitiveSuccessStackPointer := stackPointer.
	^ (aBlock value: (stackArray at: stackPointer))
		  processPrimitiveResult: self
]
