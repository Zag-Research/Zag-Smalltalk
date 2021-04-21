Class {
	#name : #PCClassBuilder,
	#superclass : #PCObject,
	#instVars : [
		'superclass',
		'name',
		'instVarNames',
		'classVariablesNames',
		'formats'
	],
	#category : #'ASTKernel-Classes'
}

{ #category : #'as yet unclassified' }
PCClassBuilder >> beBytes [
	self isCompiledMethodClassIndex
		ifTrue: [ ^ self beCompiledMethod ].
	^ formats := #(#variable #bytes)
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> beCompiledMethod [
	^ formats := #(#compiledMethod #variable #bytes)
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> bePointers [
	^ formats := #(#pointers)
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> beVariable [
	^ formats := #(#variable #pointers)
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> beWeak [
	^ formats := #(#weak #variable #pointers)
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> beWords [
	^ formats := #(#variable #words)
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> build [
	| metaclass theClass supermetaclass |
	supermetaclass := superclass
		ifNil: [ PCClass ]
		ifNotNil: [ superclass class ].
	metaclass := PCMetaclass new.
	metaclass superclass: supermetaclass.
	metaclass setFormat: supermetaclass format.
	theClass := metaclass basicNew initialize.
	theClass superclass: superclass.
	theClass setFormat: self newClassFormat.
	theClass instVarNames: instVarNames asArray.
	theClass name: name.
	theClass classVariables: PCDictionary new.
	self classVariableNames
		do: [ :varName | theClass classVariables at: varName put: nil ].
	metaclass soleInstance: theClass.
	^ theClass
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> classVariableNames [
	^ classVariablesNames
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> classVariableNames: anArray [
	classVariablesNames := anArray
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> compactClassIndex [
	^ self compactClassIndexFor: name
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> compactClassIndexFor: aClassName [
	^ #(#PCCompiledMethod nil #PCArray #PCLargeNegativeInteger #PCLargePositiveInteger #PCFloat nil #PCAssociation #PCPoint #PCRectangle #PCString #PCBlock nil #PCMethodContext nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
		indexOf: aClassName
		ifAbsent: [ 0 ]	"#PCBitmap"
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> initialize [
	super initialize.
	instVarNames := ''.
	classVariablesNames := ''.
	superclass := PCObject.
	self bePointers
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> instSize [
	^ (superclass ifNil: [ 0 ] ifNotNil: [ superclass instSize ])
		+ instVarNames size
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> instVarNames: anArray [
	instVarNames := anArray
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> isCompiledMethod [
	^ formats includes: #compiledMethod
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> isCompiledMethodClassIndex [
	^ (self compactClassIndexFor: name)
		== (self compactClassIndexFor: #PCCompiledMethod)
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> isPointers [
	^ formats includes: #pointers
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> isVariable [
	^ formats includes: #variable
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> isWeak [
	^ formats includes: #weak
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> isWords [
	^ formats includes: #words
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> name: aName [
	name := aName
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> newClassFormat [
	"<2 bits of size><5 bits of compact class index><4 bits of inst spec><6 bits of size><1 bit with a 0>"

	| size1 instSpec size2 compactClassIndex |
	size1 := (self instSize + 1) // 64 bitAnd: 16r3.
	instSpec := self isCompiledMethod
		ifTrue: [ 12 ]
		ifFalse: [ self isWeak
				ifTrue: [ 4 ]
				ifFalse: [ self isPointers
						ifTrue: [ self isVariable
								ifTrue: [ self instSize > 0
										ifTrue: [ 3 ]
										ifFalse: [ 2 ] ]
								ifFalse: [ self instSize > 0
										ifTrue: [ 1 ]
										ifFalse: [ 0 ] ] ]
						ifFalse: [ self isWords
								ifTrue: [ 6 ]
								ifFalse: [ 8 ] ] ] ].
	size2 := (self instSize + 1) \\ 64 bitAnd: 16r3F.
	compactClassIndex := self compactClassIndex.
	^ (size1 bitShift: 16) + (compactClassIndex bitShift: 11)
		+ (instSpec bitShift: 7) + (size2 bitShift: 1)
]

{ #category : #'as yet unclassified' }
PCClassBuilder >> superclass: aClass [
	superclass := aClass
]
