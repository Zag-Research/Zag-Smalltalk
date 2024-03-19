"
This class is just to extract some statistics from the running Pharo image.
"
Class {
	#name : 'ASExploreImage',
	#superclass : 'Object',
	#classVars : [
		'Info',
		'Something'
	],
	#category : 'ASTSmalltalk-Tests-Explore',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Explore'
}

{ #category : 'private - accessing' }
ASExploreImage class >> argumentsPerMethod [
	" self numericPrimitivesInImage associations sorted "

	| argumentsPerMethod |
	argumentsPerMethod := Array new: 60.
	(ProtoObject withAllSubclasses flatCollect: #methods) doWithIndex: [
		:method
		:i |
		| numArgs |
		i > 100000 ifTrue: [ ^ argumentsPerMethod ].
		numArgs := method numArgs.

		numArgs > 0 ifTrue: [
			argumentsPerMethod
				at: numArgs
				put: ((argumentsPerMethod at: numArgs)
						 ifNil: [ 1 ]
						 ifNotNil: [ :value | value + 1 ]) ] ].
	^ argumentsPerMethod
]

{ #category : 'class initialization' }
ASExploreImage class >> initialize [
	" self initialize inspect "

	| methodDefinitionsPerClass methodDefinitions implementedMethods classCount classesWithNoMethods abstractNoIvarsOrIndex ivarsAndIndex justIndexable nonVariablesSize variablesSize moreThan30 arity formats concreteNoIvarsOrIndex methodCount commonMethods |
	Info := Dictionary new.
	Info at: #symbolCount put: ByteSymbol allInstances size. "90813"
	methodDefinitionsPerClass := Bag new.
	methodDefinitions := Bag new.
	nonVariablesSize := Bag new.
	variablesSize := Bag new.
	implementedMethods := Set new.
	moreThan30 := Set new.
	classCount := 0.
	methodCount := 0.
	classesWithNoMethods := Set new.
	abstractNoIvarsOrIndex := Set new.
	concreteNoIvarsOrIndex := Set new.
	ivarsAndIndex := Set new.
	justIndexable := Set new.
	formats := Dictionary new.
	Object withAllSubclasses do: [ :class |
		methodDefinitionsPerClass add:
			((class methodDict size + 1) log * 10) asInteger.
		methodDefinitions addAll: class methodDict keys.
		(class class methodDict size = 0 and: [ class methodDict size = 0 ])
			ifTrue: [ classesWithNoMethods add: class ].
		implementedMethods addAll: class methodDict keys.
		implementedMethods addAll: class class methodDict keys.
		class isVariable
			ifTrue: [
				class instSize isZero
					ifTrue: [ justIndexable add: class ]
					ifFalse: [ ivarsAndIndex add: class ].
				variablesSize add: class instSize + 1 ]
			ifFalse: [
				class instSize isZero ifTrue: [
					class hasAbstractMethods
						ifFalse: [ concreteNoIvarsOrIndex add: class ]
						ifTrue: [ abstractNoIvarsOrIndex add: class ] ].
				nonVariablesSize add: (class instSize + 1) asLargerPowerOfTwo.
				class instSize > 44 ifTrue: [ moreThan30 add: class ] ].
		formats
			at: class format >> 16
			ifPresent: [ :set | set add: class ]
			ifAbsentPut: [ Set with: class ].
		classCount := classCount + 1.
		methodCount := methodCount + class methodDict size ].
	Info at: #formats put: formats.
	Info at: #classCount put: classCount.
	Info at: #methodCount put: methodCount.
	Info
		at: #methodDefinitionsPerClass
		put: methodDefinitionsPerClass valuesAndCounts associations sorted.
	Info
		at: #methodDefinitions
		put: methodDefinitions valuesAndCounts associations sorted.
	commonMethods := Dictionary new.
	methodDefinitions valuesAndCounts associations do: [ :a |
		commonMethods
			at: a value
			ifPresent: [ :c | c add: a key ]
			ifAbsentPut: [ OrderedCollection with: a key ] ].
	Info
		at: #commonMethods
		put: commonMethods associations sorted reversed.
	Info
		at: #variablesSize
		put: variablesSize valuesAndCounts associations sorted.
	Info
		at: #nonvariablesSize
		put: nonVariablesSize valuesAndCounts associations sorted.
	Info at: #ivarsAndIndex put: ivarsAndIndex.
	Info at: #ObjectMethods put: Object methodDict size.
	Info
		at: #classesWithNoMethods
		put: classesWithNoMethods asArray sorted.
	Info at: #moreThan30 put: moreThan30 asArray sorted.
	Info at: #implementedMethods put: implementedMethods asArray sorted.
	Info
		at: #abstractNoIvarsOrIndex
		put: abstractNoIvarsOrIndex asArray sorted.
	Info
		at: #concreteNoIvarsOrIndex
		put: concreteNoIvarsOrIndex asArray sorted.
	Info at: #justIndexable put: justIndexable asArray sorted.
	arity := Bag new.
	implementedMethods do: [ :selector | arity add: selector numArgs ].
	Info at: #arity put: arity valuesAndCounts associations sorted.
	^ Info
]

{ #category : 'private - accessing' }
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

{ #category : 'private - accessing' }
ASExploreImage class >> operatonsPerMethod [
	" self numericPrimitivesInImage associations sorted "

	| operationsPerMethod |
	operationsPerMethod := Array new: 200.
	(ProtoObject withAllSubclasses flatCollect: #methods) doWithIndex: [ :method :i |
		| numStatements |
		i > 60000 ifTrue: [ ^operationsPerMethod ].
		numStatements := method ast asSequenceNode statements size.
		
		(numStatements isNumber and: numStatements > 0) ifTrue: [
			operationsPerMethod at: numStatements 
			put: (
						(operationsPerMethod at: numStatements)
						 ifNil: [ 1 ]
						 ifNotNil: [ :value | value  + 1 ] 
					)
			].
		].
	^ operationsPerMethod
]

{ #category : 'private - accessing' }
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

{ #category : 'private - accessing' }
ASExploreImage class >> selectorsInImage [
	| imageSelectors |
	imageSelectors := Dictionary new.
	(ProtoObject withAllSubclasses flatCollect: #methods) doWithIndex: [
		:method
		:i | 
		i > 50000 ifTrue: [ ^imageSelectors ].
		imageSelectors at: method selector ifPresent: [:count| imageSelectors at: method selector put: count+1] ifAbsentPut: 1 ].
	^ imageSelectors
]
