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

{ #category : 'printing' }
ASCBasicBlock class >> canonicalArgumentName [

	^ 'aBasicBlock'
]

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
	stack := anOperation resultStack: stack basicBlock: self.
	^ false
]

{ #category : 'adding' }
ASCBasicBlock >> addSourceBlock: aBasicBlock [

	sourceBlocks add: aBasicBlock andIfNewDo: [
		sourceElements
			ifNil: [
			sourceElements := self copyStackFrom: aBasicBlock ]
			ifNotNil: [
				sourceElements
					with: aBasicBlock stack
					do: [ :stackElement :other | self flag: 'stackElement mergeWith: other' ] ] ]
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
ASCBasicBlock >> branch: aBasicBlock [

	self addOperation: (ASCBranch with: aBasicBlock).
	aBasicBlock addSourceBlock: self.
	^ aBasicBlock
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> checkForCalls [

	(sourceBlocks noneSatisfy: #endsWithCall) ifTrue: [ ^ self ].
	self primitiveChangeClassTo: ASCJoinBlock basicNew.
	stack makeStackTheSource
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
		next := ASCMethodBlock new.
		case at: class put: next.
		next addSourceBlock: self ].
	self addOperation: case.
	^ case
]

{ #category : 'copying' }
ASCBasicBlock >> copyOfStackFor: aBasicBlock [ 

	^ stack copyFor: aBasicBlock
]

{ #category : 'adding' }
ASCBasicBlock >> copyStackFrom: aBasicBlock [

	^ stack := aBasicBlock copyOfStackFor: self
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doPush: anObject [

	stack := stack pushDataAsTemporary: (AStackValue fromValue: anObject).
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
ASCBasicBlock >> methodBlock [

	^ method ifNil: [ method := sourceBlocks size > 0 ifTrue: [sourceBlocks anyOne methodBlock] ]
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> namedBlock [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASCBasicBlock >> next [

	^ operations ifEmpty: [ nil ] ifNotEmpty: [ operations last next ]
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

{ #category : 'data flow instructions' }
ASCBasicBlock >> providesNonNil: anId [

	^ (self at: anId) data isNilReference not
]

{ #category : 'API' }
ASCBasicBlock >> pushArray: arraySize [

	self addOperation: (ASCArray with: arraySize)
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

{ #category : 'initialization' }
ASCBasicBlock >> roots [

	| roots |
	roots := OrderedCollection new.
	roots add: self.
	roots addAll: self blocks.

	^ roots
]

{ #category : 'API' }
ASCBasicBlock >> send: aSymbol targetClass: target tail: tailCall [

	| next send |
	next := tailCall ifFalse: [ ASCReturnBlock new ].
	send := ASCSend new
		        selector: aSymbol;
		        return: next;
		        target: target.
	self addOperation: send.
	next ifNotNil: [ next addSourceBlock: self ].
	^ next
]

{ #category : 'accessing' }
ASCBasicBlock >> sender: sender locals: locals parameters: arguments [

	^ self subclassResponsibility
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
