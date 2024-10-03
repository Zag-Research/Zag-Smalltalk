"
I reperesent an empty stack of values within a method.
"
Class {
	#name : 'ASStack',
	#superclass : 'Collection',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'printing' }
ASStack class >> canonicalArgumentName [

	^ 'aStackElement'
]

{ #category : 'adding' }
ASStack >> add: newObject [

	^ self shouldNotImplement
]

{ #category : 'adding' }
ASStack >> addFlow: aBasicBlock [

	^ self.
]

{ #category : 'enumerating' }
ASStack >> addToCollection: aCollection [

	^ aCollection

	
]

{ #category : 'inspecting' }
ASStack >> asOrderedCollection [

	^ self addToCollection: OrderedCollection new
]

{ #category : 'displaying' }
ASStack >> displayStringOn: stream [
	self printNameOn: stream.
]

{ #category : 'enumerating' }
ASStack >> do: ignored [
]

{ #category : 'removing' }
ASStack >> drop: count [

	count = 0 ifTrue: [ ^ self ].
	self error
]

{ #category : 'testing' }
ASStack >> hasContext [

	^ false
]

{ #category : 'API' }
ASStack >> initLocalNamed: name [

	^ (ASLocalNamed new fromStack: self) name: name
]

{ #category : 'API' }
ASStack >> initParameterNamed: name [

	^ (ASParameterNamed fromStack: self) name: name; data: ASFlowParameter new
]

{ #category : 'API' }
ASStack >> initSelf [

	^ ASStackSelf fromStack: self
]

{ #category : 'inspecting' }
ASStack >> inspectionItems: aBuilder [

	<inspectorPresentationOrder: 0 title: 'Stack'>
	^ aBuilder newTable
		  addColumn: (SpStringTableColumn new
				   title: 'Stack Elements';
				   evaluated: [ :each | StObjectPrinter asTruncatedTextFrom: each ];
				   beNotExpandable;
				   beNotSortable;
				   yourself);
		  items: self asOrderedCollection;
		  yourself
]

{ #category : 'API' }
ASStack >> intoContext: anInteger [ 

]

{ #category : 'testing' }
ASStack >> isEmpty [

	^ true
]

{ #category : 'data flow instructions' }
ASStack >> makeStackTheSource [
]

{ #category : 'instance creation' }
ASStack >> newElementFor: anASCInlineBlock [ 

]

{ #category : 'accessing' }
ASStack >> offset [

	^ 0
]

{ #category : 'as yet unclassified' }
ASStack >> offsetForTemps [

	^ 0
]

{ #category : 'displaying' }
ASStack >> printNameOn: stream [

	self subclassResponsibility
]

{ #category : 'printing' }
ASStack >> printOn: aStream [

	^ aStream nextPutAll: self className
]

{ #category : 'adding' }
ASStack >> pushDataAsTemporary: aFlow [

	^ (ASStackTemporary fromStack: self) data: aFlow 
]

{ #category : 'adding' }
ASStack >> pushForTests: anObject [

	^ (ASStackTemporary fromStack: self) data: (ASFlowNew from: anObject)
]

{ #category : 'removing' }
ASStack >> remove: oldObject ifAbsent: anExceptionBlock [

	^ self shouldNotImplement
]

{ #category : 'stack' }
ASStack >> replicate [

]

{ #category : 'data flow instructions' }
ASStack >> simplifyDependenciesFor: anASCMethodBlock [

]

{ #category : 'private' }
ASStack >> species [

	^ OrderedCollection
]

{ #category : 'enumerating' }
ASStack >> with: ignored1 do: ignored2 [

	
]
