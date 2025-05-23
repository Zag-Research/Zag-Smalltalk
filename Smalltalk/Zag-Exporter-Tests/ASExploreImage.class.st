"
This class is just to extract some statistics from the running Pharo image.
"
Class {
	#name : 'ASExploreImage',
	#superclass : 'Object',
	#instVars : [
		'potentialImmediate',
		'nonLocalReturn'
	],
	#classVars : [
		'Info',
		'Something'
	],
	#category : 'Zag-Exporter-Tests-Explore',
	#package : 'Zag-Exporter-Tests',
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

{ #category : 'as yet unclassified' }
ASExploreImage class >> blockClosureTypes [
" self blockClosureTypes "
	| closures |
	closures := Bag new.
	CompiledBlock allInstancesDo: [ :cb| closures add: cb ast zagClosureClass ].
	self halt
	"	(value isVariable and: [ argument = value variable]) ifTrue: [
		variable variable zagIsLocal ifTrue: [ ^ #BlockAssignLocal ].
		variable variable zagIsInstance ifTrue: [ ^ #BlockClosure ].
		self halt.
		^ #BlockClosure ].
	(value isLiteralNode and: [ true == value value]) ifTrue: [ ^ #BlockAssignLocal:true ].
	(value isLiteralNode and: [ false == value value]) ifTrue: [ ^ #BlockAssignLocal:false ].
	self halt. 
	^ #BlockClosure
		value isMessage ifTrue: [ ^ #BlockClosure ].
"
"	statement := self statements first.
	statement isMessage ifTrue: [ ^ #BlockClosure ].
	statement isCascade ifTrue: [ ^ #BlockClosure ].
	(statement isLiteralArray and: [ statement contents size = 0 ] ) ifTrue: [ ^ statement isForByteArray ifTrue: [ #ByteArray ] ifFalse: [ #Array ] ].
	statement isReturn ifTrue: [ |value|
		value := statement value.
		value isMessage ifTrue: [ ^ #BlockClosure ].
		value isLiteralNode ifTrue: [
			value := value value.
			(value isNil or: [ value == true or: [ value == false]]) ifTrue: [ ^ #ThunkReturnImmediate ].
			self halt ].
		value isVariable ifTrue: [ |variable|
			variable := value variable.
			variable isSelfVariable ifTrue: [ ^ #ThunkReturnLocal ].
			variable isArgumentVariable ifTrue: [ ^ #ThunkReturnLocal ].
			self halt ].
		self halt ].
	statement isLiteralNode ifTrue: [ |value|
		value := statement value.
		(value isNil or: [ value == true or: [ value == false]]) ifTrue: [ ^ #ThunkImmediate ].
		self halt ].
	statement isVariable ifTrue: [ |variable|
		variable := statement variable.
		variable isCopying ifTrue: [ variable := variable originalVar ].
		variable isSelfVariable ifTrue: [ ^ #ThunkLocal ].
		variable isArgumentVariable ifTrue: [ ^ #ThunkLocal ].
		variable isTempVariable ifTrue: [ ^ #ThunkLocal ].
		variable isGlobalVariable ifTrue: [ ^ #ThunkAssociation ].
		self halt ].
	statement isAssignment ifTrue: [ |variable value|
		value := statement value.
		self halt
		].
	self halt"
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
	"compute the frequency of selectors to number of implmentors"
	Info at: #selectorImplmentationFrequence put: self selectorsInImage.
	"compute the frequence of number of operations in a method"
	Info at: #operationsPerMethodFrequence put: self operationsPerMethod.
	Info at: #sendsPerMethodFrequence put: self sendsInMethodFrequence.
	self sendStats.
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
ASExploreImage class >> operationsPerMethod [
	" self numericPrimitivesInImage associations sorted "

	| operationsPerMethod methods |
	operationsPerMethod := Array new: 200.
	methods := ProtoObject withAllSubclasses flatCollect: #methods.
	Transcript
		show: methods size;
		cr.
	methods doWithIndex: [
		:method
		:i |
		| numStatements |
		numStatements := method ast asSequenceNode statements size.

		(numStatements isNumber and: numStatements >= 0) ifTrue: [
			operationsPerMethod
				at: numStatements + 1
				put: ((operationsPerMethod at: numStatements + 1)
						 ifNil: [ 1 ]
						 ifNotNil: [ :value | value + 1 ]) ] ].
	^ operationsPerMethod
]

{ #category : 'private - accessing' }
ASExploreImage class >> primitives [
	" self primitives associations sorted "
	| primitives number |
	primitives := IdentityDictionary new.
	ASImageExporter exportMethods do: [: method |
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

	| selectorCountDictionary selectorDistribution methods |
	selectorCountDictionary := Dictionary new.
	methods := ProtoObject withAllSubclasses flatCollect: #methods.
	Transcript show: methods size; cr.
	methods doWithIndex: [
		:method
		:i |
		selectorCountDictionary
			at: method selector
			ifPresent: [ :count |
			selectorCountDictionary at: method selector put: count + 1 ]
			ifAbsentPut: 1 ].

	selectorDistribution := Array new: 1000.
	selectorCountDictionary keysAndValuesDo: [ :k :v |
		v > 1000 ifFalse: [
			selectorDistribution at: v put: ((selectorDistribution at: v)
					 ifNil: [ 1 ]
					 ifNotNil: [ :value | value + 1 ]) ] ].

	^ selectorDistribution
]

{ #category : 'private - accessing' }
ASExploreImage class >> sendStats [

	| methods sendVisitor |
	<script>
	methods := ProtoObject withAllSubclasses flatCollect: #methods.
	sendVisitor := ASSendStatsVisitor new.
	methods doWithIndex: [ :method :i | sendVisitor visit: method ast ].
	Info at: #totalSends put: sendVisitor totalSends.
	Info at: #selfSends  put: sendVisitor selfSends.
	Info at: #literalSends put: sendVisitor literalSends. 
]

{ #category : 'private - accessing' }
ASExploreImage class >> sendsInMethodFrequence [

	| selectorCountDictionary sendDistribution methods sendVisitor |
	selectorCountDictionary := Dictionary new.
	methods := ProtoObject withAllSubclasses flatCollect: #methods.
	sendVisitor := ASSendCountVisitor new.
	Transcript
		show: methods size;
		cr.

	sendDistribution := Array new: 1000.
	methods doWithIndex: [ :method :i |
		sendVisitor := ASSendCountVisitor new.
		sendVisitor visit: method ast.
		sendDistribution
			at: (sendVisitor count + 1)
			put: ((sendDistribution at: (sendVisitor count + 1))
					 ifNil: [ 1 ]
					 ifNotNil: [ :value | value + 1 ]) ].

	^ sendDistribution
]

{ #category : 'accessing' }
ASExploreImage >> foo [
	^ self bar: 2 + 3
]

{ #category : 'visiting' }
ASExploreImage >> visit: anOCMethodNode [
	^ anOCMethodNode acceptVisitor: self	
]

{ #category : 'visiting' }
ASExploreImage >> visitBlockNode: anOCBlockNode [

	potentialImmediate := anOCBlockNode statements size = 1.
	nonLocalReturn := false.
	[ anOCBlockNode statements do: [ :node| self visit: node ] ] ensure: [ potentialImmediate := false ]
]

{ #category : 'visiting' }
ASExploreImage >> visitLiteralValueNode: anOCLiteralValueNode [
]

{ #category : 'visiting' }
ASExploreImage >> visitMessageNode: anOCMessageNode [

	anOCMessageNode arguments do: [ :node| self visit: node ]
]

{ #category : 'visiting' }
ASExploreImage >> visitMethodNode: anOCMethodNode [

	anOCMethodNode statements do: [ :node| self visit: node ]
]
