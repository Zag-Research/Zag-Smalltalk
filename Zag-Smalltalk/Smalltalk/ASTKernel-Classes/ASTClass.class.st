Class {
	#name : 'ASTClass',
	#superclass : 'ASTClassDescription',
	#instVars : [
		'name',
		'instVarNames',
		'classVariables'
	],
	#category : 'ASTKernel-Classes',
	#package : 'ASTKernel-Classes'
}

{ #category : 'as yet unclassified' }
ASTClass >> classSide [
	^ self class
]

{ #category : 'as yet unclassified' }
ASTClass >> classVariables [
	"Answer the dictionary of class variables that I share with my sole instance, or nil if I have none."

	^ classVariables
]

{ #category : 'as yet unclassified' }
ASTClass >> classVariables: aDictionary [
	"Answer the dictionary of class variables that I share with my sole instance, or nil if I have none."

	^ classVariables := aDictionary
]

{ #category : 'as yet unclassified' }
ASTClass >> initFrom: aPharoClass methodDict: newMethodDict [
	"Fill in my instance variables from the given Class using the given MethodDictionary."

	superclass := ASTObject.	"corrected later"
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

{ #category : 'as yet unclassified' }
ASTClass >> instVarNames [
	"Answer an Array of the receiver's instance variable names."

	instVarNames ifNil: [ ^ #() ].
	^ instVarNames
]

{ #category : 'as yet unclassified' }
ASTClass >> instVarNames: anArray [
	instVarNames := anArray
]

{ #category : 'as yet unclassified' }
ASTClass >> instanceSide [
]

{ #category : 'as yet unclassified' }
ASTClass >> isMeta [
	^ false
]

{ #category : 'as yet unclassified' }
ASTClass >> name [
	^ name
]

{ #category : 'as yet unclassified' }
ASTClass >> name: aSymbol [
	name := aSymbol
]

{ #category : 'as yet unclassified' }
ASTClass >> newClassBuilderForSubclass: subclassName instanceVariableNames: instVarNames classVariableNames: classVarNames [
	^ ASTClassBuilder new
		superclass: self;
		name: subclassName;
		instVarNames: instVarNames;
		classVariableNames: classVarNames;
		yourself
]

{ #category : 'as yet unclassified' }
ASTClass >> subclass: subclassName instanceVariableNames: someInstVarNames classVariableNames: classVarNames [
	^ (self
		newClassBuilderForSubclass: subclassName
		instanceVariableNames: someInstVarNames
		classVariableNames: classVarNames) build
]

{ #category : 'as yet unclassified' }
ASTClass >> variableByteSubclass: subclassName instanceVariableNames: someInstVarNames classVariableNames: classVarNames [
	^ (self
		newClassBuilderForSubclass: subclassName
		instanceVariableNames: someInstVarNames
		classVariableNames: classVarNames)
		beBytes;
		build
]

{ #category : 'as yet unclassified' }
ASTClass >> variableSubclass: subclassName instanceVariableNames: someInstVarNames classVariableNames: classVarNames [
	^ (self
		newClassBuilderForSubclass: subclassName
		instanceVariableNames: someInstVarNames
		classVariableNames: classVarNames)
		beVariable;
		build
]

{ #category : 'as yet unclassified' }
ASTClass >> variableWordSubclass: subclassName instanceVariableNames: someInstVarNames classVariableNames: classVarNames [
	^ (self
		newClassBuilderForSubclass: subclassName
		instanceVariableNames: someInstVarNames
		classVariableNames: classVarNames)
		beWords;
		build
]

{ #category : 'as yet unclassified' }
ASTClass >> weakSubclass: subclassName instanceVariableNames: someInstVarNames classVariableNames: classVarNames [
	^ (self
		newClassBuilderForSubclass: subclassName
		instanceVariableNames: someInstVarNames
		classVariableNames: classVarNames)
		beWeak;
		build
]
