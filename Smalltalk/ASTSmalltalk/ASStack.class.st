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

	^ (ASLocalNamed new fromStack: self) name: name; data: ASFlowNil new
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

{ #category : 'printing' }
ASStack >> printOn: aStream [

	^ aStream nextPutAll: self className
]

{ #category : 'adding' }
ASStack >> push: anObject [

	^ (ASStackTemporary fromStack: self) data: (ASFlowNew from: anObject)
]

{ #category : 'API' }
ASStack >> pushUnknown [
	^ self push: ASCUnknownValue new
]

{ #category : 'as yet unclassified' }
ASStack >> pushValue: anObject [

	^ (ASStackTemporary fromStack: self) data: (ASFlowNew new value: anObject )
]

{ #category : 'removing' }
ASStack >> remove: oldObject ifAbsent: anExceptionBlock [

	^ self shouldNotImplement
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
