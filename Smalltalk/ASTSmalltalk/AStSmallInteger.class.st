"
I represent a `SmallInteger` value, but nothing else is known.

My subclasses represent `SmallInteger`s that we know something about.

I `couldMergeIn:` any of my subclasses - and vice versa - although the result might simply be an unbounded `AStSmallInteger`.

My `leThan` field might be `AStSizeOf`, which complicates merging
"
Class {
	#name : 'AStSmallInteger',
	#superclass : 'AStExactClass',
	#instVars : [
		'geThan',
		'leThan'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
AStSmallInteger class >> geThan: anInteger [

	^ self new geThan: anInteger
]

{ #category : 'initialization' }
AStSmallInteger class >> setValue: anInteger [

	^ self new setValue: anInteger
]

{ #category : 'comparing' }
AStSmallInteger >> = other [

	^ leThan = other leThan and: [ geThan = other geThan ]
]

{ #category : 'testing' }
AStSmallInteger >> couldMergeIn: other [

	self class == other class ifTrue: [ ^ true ].
	^ other class inheritsFrom: AStSmallInteger
]

{ #category : 'arithmetic' }
AStSmallInteger >> delta: anInteger [

	anInteger >= 0 ifTrue: [
		^ AStDelta new
			  origin: self;
			  geThan: geThan + anInteger ].
	^ AStDelta new
		  origin: self;
		  leThan: leThan + anInteger
]

{ #category : 'accessing' }
AStSmallInteger >> geThan [

	^ geThan
]

{ #category : 'accessing' }
AStSmallInteger >> geThan: anObject [

	geThan := anObject
]

{ #category : 'initialization' }
AStSmallInteger >> initialize [

	super initialize.
	dataClass := SmallInteger
]

{ #category : 'accessing' }
AStSmallInteger >> leThan [

	^ leThan
]

{ #category : 'accessing' }
AStSmallInteger >> leThan: anObject [

	^ leThan := anObject
]

{ #category : 'accessing' }
AStSmallInteger >> maxWith: anInteger [

	^ leThan ifNotNil: [ anInteger ifNotNil: [leThan max: anInteger]]
]

{ #category : 'API' }
AStSmallInteger >> mergeIn: other [

	other class ~~ AStSmallInteger ifTrue: [ ^ other mergeIn: self ].
	^ self privateMergeIn: other
]

{ #category : 'accessing' }
AStSmallInteger >> minWith: anInteger [

	^ geThan min: anInteger
]

{ #category : 'private' }
AStSmallInteger >> privateMergeIn: other [

	^ self class new
		  geThan: (self minWith: other geThan);
		  leThan: (self maxWith: other leThan);
		  yourself
]

{ #category : 'initialization' }
AStSmallInteger >> setValue: aSmallInteger [

	geThan := leThan := aSmallInteger
]

{ #category : 'evaluating' }
AStSmallInteger >> value [

	geThan = leThan ifFalse: [ ^ self error: 'not single value' ].
	^ geThan
]
