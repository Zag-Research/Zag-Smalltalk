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
		'method',
		'hash',
		'stackDepth',
		'context',
		'sourceBlocks',
		'sourceElements',
		'sequence'
	],
	#classVars : [
		'SequenceNumber'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'class initialization' }
ASCBasicBlock class >> initialize [

	SequenceNumber := 0
]

{ #category : 'comparing' }
ASCBasicBlock >> = other [

	^ hash = other hash and: [ other class = self class ]
]

{ #category : 'adding' }
ASCBasicBlock >> addBlock: aBasicBlock [

	^ self methodBlock addBlock: aBasicBlock
]

{ #category : 'initialization' }
ASCBasicBlock >> addOperation: anOperation [

	operations add: anOperation.
	stack := anOperation resultStack: stack
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
	block := self next.
	[ block notNil ] whileTrue: [
		blocks add: block.
		block := block next ].

	^ blocks
]

{ #category : 'API' }
ASCBasicBlock >> branch: anASCInlineBlock [

	self addOperation: (ASCBranch with: anASCInlineBlock).
	anASCInlineBlock addSourceBlock: self.
	^ anASCInlineBlock
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> checkForCalls [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASCBasicBlock >> children [

	^ operations
]

{ #category : 'API' }
ASCBasicBlock >> classCase: aCollection selector: aSymbol [

	| case |
	case := ASCCase with: aSymbol.
	aCollection do: [ :class |
		| next |
		next := ASCInlineBlock new.
		case at: class put: next.
		next addSourceBlock: self ].
	self addOperation: case.
	^ case
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> classForLocals [

	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> classForTemporaries [

	^ self subclassResponsibility
]

{ #category : 'copying' }
ASCBasicBlock >> copyOfStackFor: anASCInlineBlock [ 

	^ stack newElementFor: anASCInlineBlock
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doPush: anObject [

	stack := stack push: anObject.
	maxDepth := maxDepth max: (stackDepth := stackDepth + 1)
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

{ #category : 'API' }
ASCBasicBlock >> getContext [

	context ifNil: [
		stack := stack makeContextRequired: [ :c | context := c ] ].
	^ context
]

{ #category : 'testing' }
ASCBasicBlock >> hasContext [

	^ sourceElements hasContext
]

{ #category : 'testing' }
ASCBasicBlock >> hasContextOnEntry [

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

	| source |
	stack := stack initLocalNamed: aSymbol.
	source := self classForLocals new .
	^ stack
		  value: (ASCLocal new
				   init: Object;
				   name: aSymbol);
		  source: source
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
ASCBasicBlock >> inspectionZagCode [

	<inspectorPresentationOrder: 0 title: 'ZagCode'>
	^ SpTreeTablePresenter new
		  roots: operations;
		  children: [ :aNode | aNode children ];
		  addColumn:
			  (SpStringTableColumn title: 'Operations' evaluated: #contents);
		  yourself
]

{ #category : 'testing' }
ASCBasicBlock >> isRecursive: aCompileMethod [ 
	"test if the given compile method has already been visited"
	^ false
]

{ #category : 'initialization' }
ASCBasicBlock >> label [

	^ self subclassResponsibility
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
	last manifestParameters: stack
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
ASCBasicBlock >> methodBlock [

	^ method ifNil: [ method := sourceBlocks anyOne methodBlock ]
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> namedBlock [
	^ self subclassResponsibility
]

{ #category : 'as yet unclassified' }
ASCBasicBlock >> namedContext: namedBlock stack: stackBlock other: otherBlock [

	^ stack do: [ :element |
		  element namedContext: namedBlock stack: stackBlock other: otherBlock ]
]

{ #category : 'accessing' }
ASCBasicBlock >> next [

	^ operations last next
]

{ #category : 'inlining' }
ASCBasicBlock >> nextInline [

	^ nil
]

{ #category : 'successor sequences' }
ASCBasicBlock >> nextSequence [
	^ self subclassResponsibility
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

{ #category : 'initialization' }
ASCBasicBlock >> offsetTo: anObject methodBlock: aBlock [
	aBlock value: label.
	 ^ stack offset: 0 to: anObject
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

{ #category : 'API' }
ASCBasicBlock >> popVariable: aSymbol [

	self addOperation: (ASCPopVariable with: (stack at: aSymbol))
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> providesNonNil: anId [

	^ (self at: anId) source isNilReference not
]

{ #category : 'API' }
ASCBasicBlock >> pushArray: arraySize [

	self addOperation: (ASCArray with: arraySize)
]

{ #category : 'API' }
ASCBasicBlock >> pushVariable: aSymbol [

	self addOperation: (ASCPushVariable with: (stack at: aSymbol))
]

{ #category : 'accessing' }
ASCBasicBlock >> reference: anIntegerOrSymbol [

	^ (stack at: anIntegerOrSymbol) source markReferenced
]

{ #category : 'enumerating' }
ASCBasicBlock >> requiredContextDo: aBlock [

	^ context ifNotNil: [ :c | aBlock cull: c ]
]

{ #category : 'API' }
ASCBasicBlock >> returnNonLocal [

	^ self addOperation: ASCReturnNonLocal new
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

{ #category : 'abstract interpretation' }
ASCBasicBlock >> send: aSymbol targetClass: target tail: tailCall [

	| next send |
	next := tailCall ifFalse: [ ASCInlineBlock new ].
	send := ASCSend new
		        selector: aSymbol;
		        return: next;
		        target: target.
	self put: send.
	next ifNotNil: [
		next addSourceBlock: self.
		next setStack: (stack drop: aSymbol numArgs + 1) pushUnknown ].
	^ next
]

{ #category : 'initialization' }
ASCBasicBlock >> setStackForBlock: anASCBlock [

	^ anASCBlock scope: stack
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> simplifyDependencies [

	^ stack simplifyDependenciesFor: self
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
	source := ASFlowNew new.
	target source: source.
	^ source
]

{ #category : 'API' }
ASCBasicBlock >> storeVariable: aSymbol [

	self addOperation: (ASCStoreVariable with: (stack at: aSymbol))
]

{ #category : 'accessing' }
ASCBasicBlock >> top [
	^ stack top
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> updateDataDependencies: blocksToProcess [

	sourceElements do: [ :sourceElement |
		(sourceElement source isReferenced and: [
			 self anySourcesNonNilFor: sourceElement id ]) ifTrue: [
			sourceBlocks do: [ :block |
				(block reference: sourceElement id) ifTrue: [
					blocksToProcess add: block ] ] ] ]
]
