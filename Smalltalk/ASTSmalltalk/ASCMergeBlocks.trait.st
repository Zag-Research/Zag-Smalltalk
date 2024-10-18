Trait {
	#name : 'ASCMergeBlocks',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCMergeBlocks >> addFlow [ 
	| stackFromSources |
	stackFromSources := OrderedCollection new.
	self sources do: [ :source | stackFromSources add: source stack ].
	
	^ self replaceStack: (self mergeStackSources: stackFromSources).
]

{ #category : 'as yet unclassified' }
ASCMergeBlocks >> identicalLocation: stackSources [

	^ stackSources allSatisfy: [ :stack | stack location = (stackSources first location) ].
]

{ #category : 'as yet unclassified' }
ASCMergeBlocks >> mergeStackSources: stackSources [

	| newNode newLocation nextSourceLayer restOfProcessedStack |
	
	"All locations for sources are identical"
	(self identicalLocation: stackSources) ifTrue: [ ^ stackSources first. ].
	
	"Get next layer from stack elements"
	nextSourceLayer := OrderedCollection new.
	stackSources do: [ :source | nextSourceLayer add: source next. ].
	
	"Recursive call to get the rest of the processed stack"
	restOfProcessedStack := self mergeStackSources: nextSourceLayer.
	
	"Not identical, replicate stack element and add phi node"
	newLocation := AStackLocation new stackValue: (ASFlowPhi new mergeMultiple: stackSources).
	newNode := stackSources first copy location: newLocation.
	
	^ newNode fromStack: restOfProcessedStack.
]
