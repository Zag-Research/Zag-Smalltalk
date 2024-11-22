"
I am a trait that has the common code for basic blocks that are possible merge targets.
"
Trait {
	#name : 'ASCMergeBlocks',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCMergeBlocks >> addFlow [

	| stackFromSources |
	stackFromSources := self sources
		                    collect: #stack
		                    as: Array.

	^ self replaceStack: (self mergeStackSources: stackFromSources)
]

{ #category : 'as yet unclassified' }
ASCMergeBlocks >> identicalLocation: stackSources [

	^ stackSources allSatisfy: [ :stack | stack location = (stackSources first location) ].
]

{ #category : 'flow analysis' }
ASCMergeBlocks >> mergeStackSources: stackSources [

	| newNode newLocation nextSourceLayer restOfProcessedStack |
	"All locations for sources are identical"
	(self identicalLocation: stackSources) ifTrue: [
		^ stackSources first ].

	"Get next layer from stack elements"
	nextSourceLayer := stackSources
		                   collect: [ :source | source next ]
		                   as: OrderedCollection.
	restOfProcessedStack := self mergeStackSources: nextSourceLayer.

	"Not identical, replicate stack element and add phi node"
	newLocation := AStackLocation new flowValue:
		               (ASFlowPhi new mergeMultiple: stackSources).
	newNode := stackSources first copy location: newLocation.

	^ newNode fromStack: restOfProcessedStack
]
