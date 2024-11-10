"
I represent a Basic Block for ASCodeGenerator.

Every Method or block is made up of a graph of these.

"
Class {
	#name : 'ASCBasicBlock',
	#superclass : 'Object',
	#instVars : [
		'stack',
		'maxDepth',
		'operations',
		'label',
		'hash',
		'stackDepth',
		'sourceBlocks',
		'sourceElements',
		'sequence',
		'methodBlock',
		'structNumber',
		'contextStartsAt'
	],
	#classVars : [
		'SequenceNumber',
		'StructNumber'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCBasicBlock class >> canonicalArgumentName [

	^ 'aBasicBlock'
]

{ #category : 'class initialization' }
ASCBasicBlock class >> initialize [

	SequenceNumber := 0.
	StructNumber := 0
]

{ #category : 'comparing' }
ASCBasicBlock >> = other [

	^ hash = other hash and: [ other class = self class ]
]

{ #category : 'adding' }
ASCBasicBlock >> addBlock: aBasicBlock [

	^ self methodBlock addBlock: aBasicBlock
]

{ #category : 'adding' }
ASCBasicBlock >> addFlow [

	stack := stack addFlow: self.
	^ self.
]

{ #category : 'initialization' }
ASCBasicBlock >> addOperation: anOperation [
	operations add: anOperation.
	stack := anOperation resultStack: stack basicBlock: self.
	^ false
]

{ #category : 'enumerating' }
ASCBasicBlock >> afterSourcesDo: aBlock [

	^ aBlock value: self
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> anySourcesNonNilFor: id [

	^ sourceBlocks anySatisfy: [ :block | block providesNonNil: id ]
]

{ #category : 'accessing' }
ASCBasicBlock >> at: nameOrOffset [

	^ stack at: nameOrOffset
]

{ #category : 'accessing' }
ASCBasicBlock >> blocks [

	| blocks block |
	blocks := OrderedCollection new.
	block := self nextBasicBlocks.
	[ block isEmpty ] whileFalse: [
		blocks add: block.
		block := block nextBasicBlocks ].

	^ blocks
]

{ #category : 'API' }
ASCBasicBlock >> branch: aBasicBlock [

	self addOperation: (ASCBranch with: aBasicBlock).
	aBasicBlock branchedToFrom: self.
	^ aBasicBlock
]

{ #category : 'adding' }
ASCBasicBlock >> branchedToFrom: aBasicBlock [

	(sourceBlocks includes: aBasicBlock) ifTrue: [ 
		self error: 're-adding edge' ].
	sourceBlocks add: aBasicBlock andIfNewDo: [
		sourceElements
			ifNil: [ sourceElements := self copyStackFrom: aBasicBlock ]
			ifNotNil: [
				sourceElements
					with: aBasicBlock stack
					do: [ :stackElement :other |
					self flag: 'stackElement mergeWith: other' ] ] ]
]

{ #category : 'accessing' }
ASCBasicBlock >> children [

	^ operations
]

{ #category : 'API' }
ASCBasicBlock >> classCase: aCollection exhaustive: isExhaustive genertor: generator [

	| case anASCSend joinBlock numArgs |
	anASCSend := operations last.
	numArgs := anASCSend selector numArgs.
	joinBlock := anASCSend nextBasicBlocks.
	case := ASCCase with: numArgs.
	isExhaustive ifTrue: [
		self removeLastOperation.
		joinBlock becomeJoinBlock ].
	case fillFrom: aCollection with: [ :methodAST |
		(generator newInlineMethod: methodAST)
			branchedToFrom: self;
			returnBlock: joinBlock ].
	self addOperation: case.
	isExhaustive ifFalse: [ self swapLastOperations ].
	^ case
]

{ #category : 'API' }
ASCBasicBlock >> contextInherited [

	^ 0
]

{ #category : 'copying' }
ASCBasicBlock >> copyOfStackFor: aBasicBlock [

	^ stack newElementFor: aBasicBlock
]

{ #category : 'adding' }
ASCBasicBlock >> copyStackFrom: aBasicBlock [

	^ stack := aBasicBlock copyOfStackFor: self
]

{ #category : 'API' }
ASCBasicBlock >> defaultAsReturn [

	contextStartsAt ifNil: [ contextStartsAt := self contextInherited ]
]

{ #category : 'Collection API' }
ASCBasicBlock >> do: aBlock [

	^ aBlock value: self
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doPush: anObject [

	stack := stack pushDataAsTemporary: (AStackValue fromValue: anObject).
	maxDepth := maxDepth max: (stackDepth := stackDepth + 1)
]

{ #category : 'callgraph' }
ASCBasicBlock >> dotNameOn: aStream [

	^ aStream
		  << 'struct';
		  print: self structNumber
]

{ #category : 'callgraph' }
ASCBasicBlock >> dotStringOn: aStream [

	aStream << '  '.
	self dotNameOn: aStream.
	aStream
		<< '[label="{';
		<< self label.
	operations do: [ :operation |
		aStream << '|'.
		operation dotStringOn: aStream ].
	aStream << '}"];
'.
	operations last dotEdgesOn: aStream from: self
]

{ #category : 'API' }
ASCBasicBlock >> embed: aSymbol [

	self addOperation: (ASCEmbed with: aSymbol).
	^ nil
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> endsWithCall [

	operations ifEmpty: [ ^ false ].
	^ operations last isCallOperation
]

{ #category : 'accessing' }
ASCBasicBlock >> flow [

	^ false
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> flowForLocals [

	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> flowForParameters [

	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> flowForSelf [

	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> flowForTemporaries [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCBasicBlock >> forceCreateContext [

	contextStartsAt = self noContext ifTrue: [ self insertContext ]
]

{ #category : 'testing' }
ASCBasicBlock >> hasContext [

	^ sourceElements hasContext
]

{ #category : 'comparing' }
ASCBasicBlock >> hash [

	^ hash
]

{ #category : 'comparing' }
ASCBasicBlock >> hash: value [

	hash := value
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> indexOf: anObject [
"delegate to stack?"
	^ stack offset: 0 to: anObject 

]

{ #category : 'API' }
ASCBasicBlock >> initLocalNamed: aSymbol [

	^ stack := stack initLocalNamed: aSymbol".
	^ stack data: (self flowForLocals new
			   basicBlock: self;
			   class: nil)"
]

{ #category : 'initialization' }
ASCBasicBlock >> initialize [

	super initialize.
	stackDepth := maxDepth := 0.
	stack := ASStack new.
	hash := SequenceNumber := SequenceNumber + 1.
	sourceBlocks := Set new.
	operations := OrderedCollection new
]

{ #category : 'inlining' }
ASCBasicBlock >> inlineIfPossible: anASCodeGenerator [

	operations ifEmpty: [ ^ self ].
	^ operations last inlineIfPossible: anASCodeGenerator
]

{ #category : 'inlining' }
ASCBasicBlock >> inlineWithFixedCFG: anASCodeGenerator [

	operations ifEmpty: [ ^ self ].
	^ operations last inlineWithFixedCFG: anASCodeGenerator
]

{ #category : 'API' }
ASCBasicBlock >> insertContext [

	self halt.
	contextStartsAt ifNotNil: [ contextStartsAt = self noContext ifFalse: [ ^ self ]].
	operations withIndexDo: [ :operation :index |
		operation shrinksStack ifFalse: [
			operations
				add:
				(ASCCreateContext new
					 resultStack: operation stack
					 basicBlock: self;
					yourself)
				beforeIndex: index.
			contextStartsAt := index.
			^ self ] ]
]

{ #category : 'printing' }
ASCBasicBlock >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		<< $(;
		<< self label;
		<< $)
]

{ #category : 'writing' }
ASCBasicBlock >> inspectionStack: aBuilder [

	<inspectorPresentationOrder: 1 title: 'Stack'>
	^ aBuilder newTable
		  addColumn: (SpStringTableColumn new
				   title: 'Stack Elements';
				   evaluated: [ :each | StObjectPrinter asTruncatedTextFrom: each ];
				   beNotExpandable;
				   beNotSortable;
				   yourself);
		  items: stack asOrderedCollection;
		  yourself

]

{ #category : 'writing' }
ASCBasicBlock >> inspectionStart: aBuilder [

	<inspectorPresentationOrder: 1 title: 'Source'>
	^ aBuilder newTable
		  addColumn: (SpStringTableColumn new
				   title: 'Stack Elements';
				   evaluated: [ :each | StObjectPrinter asTruncatedTextFrom: each ];
				   beNotExpandable;
				   beNotSortable;
				   yourself);
		  items: self sourceElementsForInspect;
		  yourself
]

{ #category : 'writing' }
ASCBasicBlock >> inspectionZagCode [

	<inspectorPresentationOrder: 0 title: 'ZCode'>
	^ SpTreeTablePresenter new
		  roots: operations;
		  children: [ :aNode | aNode children ];
		  addColumn:
			  (SpStringTableColumn title: 'Operations' evaluated: #contents);
		  yourself
]

{ #category : 'Collection API' }
ASCBasicBlock >> isEmpty [

	^ false
]

{ #category : 'initialization' }
ASCBasicBlock >> label [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASCBasicBlock >> label: aString [ 
	label := aString
]

{ #category : 'output' }
ASCBasicBlock >> llvmLabel [
	^ self subclassResponsibility
]

{ #category : 'inlining' }
ASCBasicBlock >> markBlockClosuresToManifest [

	| last |
	last := operations last.
	last requiresContext ifTrue: [ self getContext ].
	last manifestParameters
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> markReferences [

	operations do: #markReferences
]

{ #category : 'accessing' }
ASCBasicBlock >> maxDepth [
	^ maxDepth
]

{ #category : 'accessing' }
ASCBasicBlock >> merge [ 
	
	"recurse over sources"
	"generate phi for all elements until we hit a 'common path' - first thing that is not an ASFlowNew (?)"
	"the stack of the basicBlock should contain phis + common path"
]

{ #category : 'accessing' }
ASCBasicBlock >> methodBlock [

	^ methodBlock ifNil: [ methodBlock := sourceBlocks size > 0 ifTrue: [sourceBlocks anyOne methodBlock] ]
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> namedBlock [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASCBasicBlock >> nextBasicBlocks [

	^ operations ifEmpty: [ nil ] ifNotEmpty: [ operations last nextBasicBlocks ]
]

{ #category : 'successor sequences' }
ASCBasicBlock >> nextSequence [
	^ self subclassResponsibility
]

{ #category : 'as yet unclassified' }
ASCBasicBlock >> noContext [

	^ SmallInteger maxVal
]

{ #category : 'API' }
ASCBasicBlock >> offsetOf: aSymbol context: blockLocal stack: blockStack other: blockOther [

	^ stack
		  offset: 0
		  to: aSymbol
		  context: blockLocal
		  stack: blockStack
		  other: blockOther
]

{ #category : 'initialization' }
ASCBasicBlock >> offsetTo: anObject [

	^ stack offset: 0 to: anObject
]

{ #category : 'API' }
ASCBasicBlock >> operationAt: anInteger [ 

	^ operations at: anInteger
]

{ #category : 'writing' }
ASCBasicBlock >> outputRestTo: output [

	operations do: [ :operation | operation outputTo: output ].
	^ output outputBasicBlockEnd
]

{ #category : 'writing' }
ASCBasicBlock >> outputTo: output [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCBasicBlock >> popAndCopyTopToSelf [

	self addOperation: (ASCPopAndCopyTop new)
]

{ #category : 'stack' }
ASCBasicBlock >> popStackTo: anASCMethodBlock [

	self addOperation: (ASCCopyVariablesCleanStack new
			 target: anASCMethodBlock sourceElements
			 source: stack)
]

{ #category : 'API' }
ASCBasicBlock >> popVariable: aSymbol [

	self addOperation: (ASCPopVariable with: (stack at: aSymbol))
]

{ #category : 'printing' }
ASCBasicBlock >> printOn: aStream [

	aStream nextPutAll: self label
]

{ #category : 'API' }
ASCBasicBlock >> propagateContextFreeFor: aStackContext [

	contextStartsAt ifNil: [
		contextStartsAt := self noContext.
		(operations anySatisfy: #needsContext) ifTrue: [
			self insertContext.
			aStackContext growBy: 0.
			^ self ] ]
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> providesNonNil: anId [

	^ (self at: anId) data isNilReference not
]

{ #category : 'API' }
ASCBasicBlock >> pushArray: arraySize [

	self addOperation: (ASCArray with: arraySize)
]

{ #category : 'API' }
ASCBasicBlock >> pushLiteral: aLiteral [

	^ self addOperation: (ASCLiteral with: aLiteral)
]

{ #category : 'API' }
ASCBasicBlock >> pushNil [

	stack := stack pushDataAsTemporary: AStUndefinedClass singleton
]

{ #category : 'API' }
ASCBasicBlock >> pushVariable: aSymbol [

	self addOperation: (ASCPushVariable with: (stack at: aSymbol))
]

{ #category : 'testing' }
ASCBasicBlock >> recursiveBasicBlock: aCompileMethod [ 
	"test if the given compile method has already been visited"
	^ false
]

{ #category : 'accessing' }
ASCBasicBlock >> reference: anIntegerOrSymbol [

	^ (stack at: anIntegerOrSymbol) data markReferenced
]

{ #category : 'removing' }
ASCBasicBlock >> removeLastOperation [

	operations removeLast
]

{ #category : 'inlining' }
ASCBasicBlock >> replaceOperation: anOperation [

	operations
		at: operations size
		put: (anOperation fromOperation: (operations at: operations size))
]

{ #category : 'as yet unclassified' }
ASCBasicBlock >> replaceStack: newStack [
	stack := newStack.
	^ self.
]

{ #category : 'API' }
ASCBasicBlock >> requireSourceContext [

	sourceBlocks do: [ : source | source forceCreateContext ]
]

{ #category : 'API' }
ASCBasicBlock >> returnSelf [

	^ self addOperation: ASCReturnSelf new
]

{ #category : 'API' }
ASCBasicBlock >> returnTop [

	^ self addOperation: (ASCReturnTop new)
]

{ #category : 'initialization' }
ASCBasicBlock >> roots [

	| roots |
	roots := OrderedCollection new.
	roots add: self.
	roots addAll: self blocks.

	^ roots
]

{ #category : 'API' }
ASCBasicBlock >> send: aSymbol targetClass: target [

	| next send |
	next := ASCReturnBlock new.
	send := ASCSend new
		        selector: aSymbol;
		        return: next;
		        target: target.
	self addOperation: send.
	next branchedToFrom: self.
	^ next
]

{ #category : 'API' }
ASCBasicBlock >> sendTail: aSymbol targetClass: target [

	| send |
	send := ASCTailSend new
		        selector: aSymbol;
		        target: target.
	self addOperation: send.
	^ nil
]

{ #category : 'initialization' }
ASCBasicBlock >> setStackForBlock: anASCBlock [

	^ anASCBlock scope: stack
]

{ #category : 'accessing' }
ASCBasicBlock >> sourceElements [

	^ sourceElements
]

{ #category : 'writing' }
ASCBasicBlock >> sourceElementsForInspect [

	^ sourceElements asOrderedCollection
]

{ #category : 'as yet unclassified' }
ASCBasicBlock >> sources [ 
	^ sourceBlocks.
]

{ #category : 'accessing' }
ASCBasicBlock >> stack [
	^ stack
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> stackBelow: n [

	^ stack drop: n
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> stackTop [
	^ stack top
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> storeInto: offsetOrName [

	| target source |
	target := stack at: offsetOrName.
	source := ASFlowNew new basicBlock: self.
	target data: source.
	^ source
]

{ #category : 'API' }
ASCBasicBlock >> storeVariable: aSymbol [

	self addOperation: (ASCStoreVariable with: (stack at: aSymbol))
]

{ #category : 'as yet unclassified' }
ASCBasicBlock >> structNumber [

	^ structNumber ifNil: [ structNumber := StructNumber := StructNumber + 1 ]
]

{ #category : 'inlining' }
ASCBasicBlock >> swapLastOperations [

	| size temp |
	size := operations size.
	temp := operations at: size.
	operations at: size put: (operations at: size - 1).
	operations at: size - 1 put: temp
]

{ #category : 'accessing' }
ASCBasicBlock >> top [
	^ stack top
]
