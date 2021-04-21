Class {
	#name : #ASTClassBuilder,
	#superclass : #ASTObject,
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
ASTClassBuilder >> beBytes [
	self isCompiledMethodClassIndex
		ifTrue: [ ^ self beCompiledMethod ].
	^ formats := #(#variable #bytes)
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> beCompiledMethod [
	^ formats := #(#compiledMethod #variable #bytes)
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> bePointers [
	^ formats := #(#pointers)
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> beVariable [
	^ formats := #(#variable #pointers)
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> beWeak [
	^ formats := #(#weak #variable #pointers)
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> beWords [
	^ formats := #(#variable #words)
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> build [
	| metaclass theClass supermetaclass |
	supermetaclass := superclass
		ifNil: [ ASTClass ]
		ifNotNil: [ superclass class ].
	metaclass := ASTMetaclass new.
	metaclass superclass: supermetaclass.
	metaclass setFormat: supermetaclass format.
	theClass := metaclass basicNew initialize.
	theClass superclass: superclass.
	theClass setFormat: self newClassFormat.
	theClass instVarNames: instVarNames asArray.
	theClass name: name.
	theClass classVariables: ASTDictionary new.
	self classVariableNames
		do: [ :varName | theClass classVariables at: varName put: nil ].
	metaclass soleInstance: theClass.
	^ theClass
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> classVariableNames [
	^ classVariablesNames
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> classVariableNames: anArray [
	classVariablesNames := anArray
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> compactClassIndex [
	^ self compactClassIndexFor: name
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> compactClassIndexFor: aClassName [
	^ #(#ASTCompiledMethod nil #ASTArray #ASTLargeNegativeInteger #ASTLargePositiveInteger #ASTFloat nil #ASTAssociation #ASTPoint #ASTRectangle #ASTString #ASTBlock nil #ASTMethodContext nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
		indexOf: aClassName
		ifAbsent: [ 0 ]	"#ASTBitmap"
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> initialize [
	super initialize.
	instVarNames := ''.
	classVariablesNames := ''.
	superclass := ASTObject.
	self bePointers
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> instSize [
	^ (superclass ifNil: [ 0 ] ifNotNil: [ superclass instSize ])
		+ instVarNames size
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> instVarNames: anArray [
	instVarNames := anArray
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> isCompiledMethod [
	^ formats includes: #compiledMethod
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> isCompiledMethodClassIndex [
	^ (self compactClassIndexFor: name)
		== (self compactClassIndexFor: #ASTCompiledMethod)
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> isPointers [
	^ formats includes: #pointers
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> isVariable [
	^ formats includes: #variable
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> isWeak [
	^ formats includes: #weak
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> isWords [
	^ formats includes: #words
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> name: aName [
	name := aName
]

{ #category : #'as yet unclassified' }
ASTClassBuilder >> newClassFormat [
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
ASTClassBuilder >> superclass: aClass [
	superclass := aClass
]
