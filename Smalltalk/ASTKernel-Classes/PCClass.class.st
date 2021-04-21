Class {
	#name : #PCClass,
	#superclass : #PCBehavior,
	#instVars : [
		'name',
		'instVarNames',
		'classVariables'
	],
	#category : #'ASTKernel-Classes'
}

{ #category : #'as yet unclassified' }
PCClass >> classSide [
	^ self class
]

{ #category : #'as yet unclassified' }
PCClass >> classVariables [
	"Answer the dictionary of class variables that I share with my sole instance, or nil if I have none."

	^ classVariables
]

{ #category : #'as yet unclassified' }
PCClass >> classVariables: aDictionary [
	"Answer the dictionary of class variables that I share with my sole instance, or nil if I have none."

	^ classVariables := aDictionary
]

{ #category : #'as yet unclassified' }
PCClass >> initFrom: aPharoClass methodDict: newMethodDict [
	"Fill in my instance variables from the given Class using the given MethodDictionary."

	superclass := PCObject.	"corrected later"
	methodDict := newMethodDict.
	format := aPharoClass format.
	name := (aPharoClass name copyFrom: 2 to: aPharoClass name size)
		asSymbol.	"omit leading M"
	instVarNames := aPharoClass instVarNames.
	classVariables := aPharoClass classPool.
	instVarNames size = 0
		ifTrue: [ instVarNames := nil ].
	classVariables size = 0
		ifTrue: [ classVariables := nil ]
]

{ #category : #'as yet unclassified' }
PCClass >> instVarNames [
	"Answer an Array of the receiver's instance variable names."

	instVarNames ifNil: [ ^ #() ].
	^ instVarNames
]

{ #category : #'as yet unclassified' }
PCClass >> instVarNames: anArray [
	instVarNames := anArray
]

{ #category : #'as yet unclassified' }
PCClass >> instanceSide [
]

{ #category : #'as yet unclassified' }
PCClass >> isMeta [
	^ false
]

{ #category : #'as yet unclassified' }
PCClass >> name [
	^ name
]

{ #category : #'as yet unclassified' }
PCClass >> name: aSymbol [
	name := aSymbol
]

{ #category : #'as yet unclassified' }
PCClass >> newClassBuilderForSubclass: subclassName instanceVariableNames: instVarNames classVariableNames: classVarNames [
	^ PCClassBuilder new
		superclass: self;
		name: subclassName;
		instVarNames: instVarNames;
		classVariableNames: classVarNames;
		yourself
]

{ #category : #'as yet unclassified' }
PCClass >> subclass: subclassName instanceVariableNames: someInstVarNames classVariableNames: classVarNames [
	^ (self
		newClassBuilderForSubclass: subclassName
		instanceVariableNames: someInstVarNames
		classVariableNames: classVarNames) build
]

{ #category : #'as yet unclassified' }
PCClass >> variableByteSubclass: subclassName instanceVariableNames: someInstVarNames classVariableNames: classVarNames [
	^ (self
		newClassBuilderForSubclass: subclassName
		instanceVariableNames: someInstVarNames
		classVariableNames: classVarNames)
		beBytes;
		build
]

{ #category : #'as yet unclassified' }
PCClass >> variableSubclass: subclassName instanceVariableNames: someInstVarNames classVariableNames: classVarNames [
	^ (self
		newClassBuilderForSubclass: subclassName
		instanceVariableNames: someInstVarNames
		classVariableNames: classVarNames)
		beVariable;
		build
]

{ #category : #'as yet unclassified' }
PCClass >> variableWordSubclass: subclassName instanceVariableNames: someInstVarNames classVariableNames: classVarNames [
	^ (self
		newClassBuilderForSubclass: subclassName
		instanceVariableNames: someInstVarNames
		classVariableNames: classVarNames)
		beWords;
		build
]

{ #category : #'as yet unclassified' }
PCClass >> weakSubclass: subclassName instanceVariableNames: someInstVarNames classVariableNames: classVarNames [
	^ (self
		newClassBuilderForSubclass: subclassName
		instanceVariableNames: someInstVarNames
		classVariableNames: classVarNames)
		beWeak;
		build
]
