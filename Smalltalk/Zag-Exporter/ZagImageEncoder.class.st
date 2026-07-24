"
I am an abstract class to show all the methods encoders need.
"
Class {
	#name : 'ZagImageEncoder',
	#superclass : 'Object',
	#classVars : [
		'AgeStatic',
		'BaseAddress',
		'FormatDirectIndexed',
		'FormatExternal',
		'FormatExternalNonObject',
		'FormatExternalWeakWithPointers',
		'FormatIndexed',
		'FormatIndexedNonObject',
		'FormatIndexedWeakWithPointers',
		'FormatNotIndexable',
		'MaxHeaderLength'
	],
	#classInstVars : [
		'zagClassMap',
		'zagThreadedMap'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'converting' }
ZagImageEncoder class >> boolean: value [ 
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ZagImageEncoder class >> codeAddresses [

	^ DoubleWordArray withAll: (self zagThreadesFns collectWithIndex: [ :ignored :index |
		  index * 8 ])
]

{ #category : 'converting' }
ZagImageEncoder class >> convertToZag: anObject [

	^ anObject convertToZag: self
]

{ #category : 'converting' }
ZagImageEncoder class >> double: aNumber for: anImage [

	^ self subclassResponsibility
]

{ #category : 'class initialization' }
ZagImageEncoder class >> initializeClassConstants [

	zagClassMap := Dictionary newFromPairs: self zagClasses
]

{ #category : 'class initialization' }
ZagImageEncoder class >> initializeHeapConstants [

	AgeStatic := 7.
	BaseAddress := 16r100000.
	MaxHeaderLength := 4092.
	FormatNotIndexable := 112.
	FormatIndexedNonObject := 113.
	FormatExternalNonObject := 114.
	FormatDirectIndexed := 115.
	FormatIndexed := 116.
	FormatExternal := 117.
	FormatExternalWeakWithPointers := 126.
	FormatIndexedWeakWithPointers := 127
]

{ #category : 'class initialization' }
ZagImageEncoder class >> initializeThreadedConstants [

	zagThreadedMap := Dictionary new.
	self zagThreadesFns doWithIndex: [ :symbol :index |
		zagThreadedMap at: symbol put: index * 8 ]
]

{ #category : 'converting' }
ZagImageEncoder class >> literal: l class: c [

	^ self subclassResponsibility
]

{ #category : 'magic' }
ZagImageEncoder class >> magic [

	^ self subclassResponsibility 
]

{ #category : 'converting' }
ZagImageEncoder class >> pointer: p [

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> smallInteger: aNumber [

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> symbolArity: arity hash: hash [

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> undefinedObject [
	^ self subclassResponsibility
]

{ #category : 'class initialization' }
ZagImageEncoder class >> updateClassVariables: mapping withCriteria: criteria [
	" remove unused class variables and add new ones "

	" remove current class variables that are no longer used "
	(self classVarNames select: [ :each |
		 (criteria value: each) and: [ (mapping includesKey: each) not ] ])
		do: [ :each | self removeClassVarNamed: each ].
	mapping keysDo: [ :each |
			| binding |
			"Check if the class variable already exists to avoid duplicates"
			(self hasClassVarNamed: each) ifFalse: [
				self addClassVarNamed: each ].
			binding := self classPool
				           associationAt: each
				           ifAbsent: [
				           self error:
					           'Class variable not found: ' , each asString ].
			binding value: (mapping at: each) ]
]

{ #category : 'zag runtime alignment' }
ZagImageEncoder class >> zagClasses [

	^ self subclassResponsibility
]

{ #category : 'zag runtime alignment' }
ZagImageEncoder class >> zagThreadesFns [

	^ self subclassResponsibility
]
