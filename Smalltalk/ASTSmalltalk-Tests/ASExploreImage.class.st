"
This class is just to extract some statistics from the running Pharo image.
"
Class {
	#name : #ASExploreImage,
	#superclass : #Object,
	#classVars : [
		'Info',
		'Something'
	],
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #'class initialization' }
ASExploreImage class >> initialize [
	" self initialize inspect "
	| methodDefinitions implementedMethods classCount classesWithNoMethods noIvarsOrIndex ivarsAndIndex justIndexable nonVariablesSize variablesSize moreThan30 arity formats |
	Info := Dictionary new.
	Info at: #symbolCount put: ByteSymbol allInstances size "90813".
	methodDefinitions := Bag new.
	nonVariablesSize := Bag new.
	variablesSize := Bag new.
	implementedMethods :=Set new.
	moreThan30 := Set new.
	classCount := 0.
	classesWithNoMethods := Set new.
	noIvarsOrIndex := Set new.
	ivarsAndIndex := Set new.
	justIndexable := Set new.
	formats := Dictionary new.
	Object withAllSubclasses do: [: class |
		methodDefinitions add: ((class methodDict size+1) log*10) asInteger.
		(class class methodDict size = 0 and: [class methodDict size = 0]) ifTrue: [ classesWithNoMethods add: class].
		implementedMethods addAll: class methodDict keys. implementedMethods addAll: class class methodDict keys.
		class isVariable ifTrue: [
			class instSize isZero ifTrue: [ justIndexable add: class ] ifFalse: [ ivarsAndIndex add: class ].
			variablesSize add: class instSize + 1.
		] ifFalse: [
			class instSize isZero ifTrue: [ noIvarsOrIndex add: class ].
			nonVariablesSize add: (class instSize + 1) asLargerPowerOfTwo.
			class instSize > 44 ifTrue: [ moreThan30 add: class ].
		].
		formats at: class format >> 16 ifPresent: [: set | set add: class ] ifAbsentPut: [ Set with: class ].
		classCount := classCount + 1.
	].
	Info at: #formats put: formats.
	Info at: #classCount put: classCount.
	Info at: #methodDefinitionsPerClass put: methodDefinitions valuesAndCounts associations sorted.
	Info at: #variablesSize put: variablesSize valuesAndCounts associations sorted.
	Info at: #nonvariablesSize put: nonVariablesSize valuesAndCounts associations sorted.
	Info at: #ObjectMethods put:	Object methodDict size.
	Info at: #classesWithNoMethods put: classesWithNoMethods asArray sorted.
	Info at: #moreThan30 put: moreThan30 asArray sorted.
	Info at: #implementedMethods put: implementedMethods asArray sorted.
	Info at: #noIvarsOrIndex put: noIvarsOrIndex asArray sorted.
	Info at: #ivarsAndIndex put: ivarsAndIndex asArray sorted.
	Info at: #justIndexable put: justIndexable asArray sorted.
	arity := Bag new.
	implementedMethods do: [ : selector | arity add: selector numArgs ].
	Info at: #arity put: arity valuesAndCounts associations sorted.
	^ Info
]

{ #category : #'private - accessing' }
ASExploreImage class >> numericPrimitivesInImage [
	" self numericPrimitivesInImage associations sorted "
	| primitives number |
	primitives := IdentityDictionary new.
	(ProtoObject withAllSubclasses flatCollect: #methods) do: [: method |
		method pragmas do: [: pragma |
			pragma selector = #primitive: ifTrue: [
				number := pragma arguments first.
				number isNumber ifTrue: [primitives at: number
					ifPresent: [: oc | oc add: method ]
					ifAbsentPut: [ OrderedCollection with: method ]]
			]
		]
	].
	^ primitives
]

{ #category : #'private - accessing' }
ASExploreImage class >> primitives [
	" self primitives associations sorted "
	| primitives number |
	primitives := IdentityDictionary new.
	ASImage exportMethods do: [: method |
		method pragmas do: [: pragma |
			pragma selector = #primitive: ifTrue: [
				number := pragma arguments first.
				primitives at: number
					ifPresent: [: oc | oc add: method ]
					ifAbsentPut: [ OrderedCollection with: method ]
			]
		]
	].
	^ primitives
]
