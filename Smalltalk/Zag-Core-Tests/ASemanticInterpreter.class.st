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
		'basicBlocks'
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

{ #category : 'as yet unclassified' }
ASemanticInterpreter >> afterTailSend [

	tailSend := false.
	contextData := nil
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

{ #category : 'as yet unclassified' }
ASemanticInterpreter >> beforeTailSend [

	context sendingContext ifNotNil: [ context := context sendingContext ]
]

{ #category : 'primitive execution' }
ASemanticInterpreter >> binary: aBlock [

	primitiveSuccessStackPointer := stackPointer + 1.
	^ (aBlock
		   value: (stackArray at: stackPointer + 1)
		   value: (stackArray at: stackPointer)) processPrimitiveResult:
		  self
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
ASemanticInterpreter >> contextDataAt: anInteger [ 
	^ contextData at: 1+ anInteger 
]

{ #category : 'accessing' }
ASemanticInterpreter >> copiledUnitAt: anInteger [ 
	self shouldBeImplemented.
]

{ #category : 'interpreting' }
ASemanticInterpreter >> createContext [
	"A context will be dynamically created before every a pop of a local or a send operation occurs.
Only one context will be created for every method.
Reading of a local before a context is created will result in a nil.
Locals do not need to be factored into the creation of a context."

	| size aCompiledMethodOrBlock newStackArray |

	aCompiledMethodOrBlock := pc basicBlock.
	"calculate the size of the stack to copy over, locals should not be on the stack at this time"
	size := aCompiledMethodOrBlock arguments + 1.

	"copy the arguments to the call into a newStack"
	newStackArray := Array new: 10.
	size to: 1 by: -1 do: [ :idx |
	newStackArray at: 11 - idx put: self pop ].

	"copy the current calls data into a contextData"
	context := ASIContext new
		           contextData: (context createContextData: self);
		           returnPC: pc;
		           method: aCompiledMethodOrBlock;
		           sendingContext: context.
	"why nil out the stack? visual"
	"contextPtr + 1 to: contextPtr + locals do: [ :idx |
	stackArray at: idx put: nil ]"
	stackArray := newStackArray.
	stackPointer := 11 - size.


]

{ #category : 'interpreting' }
ASemanticInterpreter >> createContext: locals extra: extra [

	| contextPtr size |
	contextPtr := stackPointer - locals - 1.
	size := BaseAddress - (contextPtr + 1).
	
	context := ASIContext new
		           array: stackArray offset: contextPtr size: size;
		           sendingContext: context;
		           extra: extra.
	
	""
	stackPointer := contextPtr - extra.
	stackArray at: contextPtr put: context.
	
	""
	stackPointer to: contextPtr - 1 do: [ :idx |
		stackArray at: idx put: nil ].
	contextPtr + 1 to: contextPtr + locals do: [ :idx |
		stackArray at: idx put: nil ]
]

{ #category : 'interpreting' }
ASemanticInterpreter >> createContextData: aMethod [
	contextDataMethod == aMethod ifTrue: [ ^ contextData ].

	contextDataMethod := aMethod.
	contextData := Array new: aMethod arguments + aMethod locals + 1.
	stackPointer to: 10 by: 1 do: [ :idx |
			| element |
			contextData at: 11 - idx put: (stackArray at: idx) ].

	^ contextData
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
ASemanticInterpreter >> installContextIfNone [
	""""
	"| newContext |
	newContext := ASIContext new.
	newContext sendingContext: context.
	newContext populateFrom: self.
	context := newContext."
	self createContext
]

{ #category : 'interpreting' }
ASemanticInterpreter >> installContextIfNone: argsOnStack [
	""""

	"| newContext |
	newContext := ASIContext new.
	newContext sendingContext: context.
	newContext populateFrom: self.
	context := newContext."

	self createContext
]

{ #category : 'interpreting' }
ASemanticInterpreter >> interpret [

	executing := true.
	[ executing ] whileTrue: [ pc doForInterpreter: self ]
]

{ #category : 'accessing' }
ASemanticInterpreter >> pop [

	stackPointer := stackPointer + 1. "move the stack pointer one before current"
	^ stackArray at: stackPointer - 1"return whatever the stack was pointing to"
]

{ #category : 'accessing' }
ASemanticInterpreter >> pop: anObject into: offset [

	stackPointer := stackPointer + 1. "remove the value on the top of the stack"
	^ stackArray at: stackArray size - offset put: anObject
]

{ #category : 'primitive execution' }
ASemanticInterpreter >> primitiveSuccess: anObject [

	stackPointer := primitiveSuccessStackPointer.
	stackArray at: stackPointer put: anObject.

	context sendingContext ifNotNil: [ context := context sendingContext ].
	pc := context returnPC.

	contextData := context contextData
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
	self addClasses: {
			#ThunkHeap.
			#ThunkReturnLocal.
			#ThunkReturnObject.
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
			nil.
			nil.
			nil.
			nil.
			nil.
			nil.
			nil.
			nil.
			nil.
			nil.
			nil.
			nil.
			nil.
			nil.
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
ASemanticInterpreter >> resetBeforeCall [
	context returnPC: context sendingContext returnPC.
	contextData := nil
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
		sources := sources bitShift: -8 ].
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

	^ self
		  restructure: anInteger
		  stack: stackPointer + (anInteger bitAnd: 16rff) - 1
]

{ #category : 'handling' }
ASemanticInterpreter >> return: anObject selfContext: contextOffset [

	stackPointer := stackArray size + contextOffset + 1.
	stackArray at: stackPointer put: anObject.
	pc := context sendingContext returnPC
]

{ #category : 'handling' }
ASemanticInterpreter >> return: anObject selfOffset: selfOffset [

	stackPointer := stackPointer + selfOffset + 1.
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

	self reset.
	self push: anObject.
	arguments do: [ :arg | self push: arg ].
	self send: aSymbol.
	context := ASIContext new returnPC: ASINullProgramCounter new.
	self interpret.
	^ self top
]

{ #category : 'accessing' }
ASemanticInterpreter >> stackAt: anOffset [
	"(tailSend or: (contextData = nil)) ifTrue: [ 
		^ stackArray at: stackPointer + anOffset ].
	
	^ contextData at: (anOffset + 1)"
	^ stackArray at: stackPointer + anOffset
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

{ #category : 'accessing' }
ASemanticInterpreter >> varAt: aStackElement [ 
	contextData ifNotNil: [
		self halt. 
	 ^ self contextDataAt: aStackElement offset
	].
	^self stackAt: aStackElement offset.
]
