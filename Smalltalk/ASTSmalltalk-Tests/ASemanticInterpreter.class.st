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
	basicBlocks addAll: cg basicBlocks.
	^ cg basicBlocks first
]

{ #category : 'private - testing' }
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

{ #category : 'accessing' }
ASemanticInterpreter >> pop [

	^ stack removeLast
]

{ #category : 'accessing' }
ASemanticInterpreter >> push: anObject [

	^ stack addLast: anObject 


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

{ #category : 'handling' }
ASemanticInterpreter >> return: anObject selfOffset: anInteger [

	context ifNil: [
		stack removeLast: anInteger.
		self assert: stack size = 1.
		stack at: 1 put: anObject.
		executing := false.
		^ self ].
	self shouldBeImplemented
]

{ #category : 'accessing' }
ASemanticInterpreter >> returnTo: aBasicBlock [

	pc := ASIProgramCounter new forBasicBlock: aBasicBlock
]

{ #category : 'interpreting' }
ASemanticInterpreter >> send: aSymbol [

	| obj method |
	obj := stack at: stack size - aSymbol numArgs.
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
	stack addLast: anObject.
	arguments do: [ :arg | stack addLast: arg ].
	self send: aSymbol.
	self interpret.
	^ stack last
]

{ #category : 'accessing' }
ASemanticInterpreter >> stackAt: anInteger [

	^ stack at: stack size - anInteger 
]

{ #category : 'private - testing' }
ASemanticInterpreter >> stackDepth [

	^ stack size
]

{ #category : 'accessing' }
ASemanticInterpreter >> top [

	^ stack last
]
