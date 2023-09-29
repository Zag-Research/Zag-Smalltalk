Class {
	#name : #ASZigFileContext,
	#superclass : #Object,
	#instVars : [
		'symbols',
		'classNumber'
	],
	#classVars : [
		'Predefined'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #testing }
ASZigFileContext >> addSymbol: aSymbol [

	symbols add: aSymbol
]

{ #category : #initialization }
ASZigFileContext >> initialize [

	super initialize.
	symbols := ASZigSymbols new.
	classNumber := 0
]

{ #category : #testing }
ASZigFileContext >> predefinedSymbol: aSymbol [

	^ (symbols predefinedSymbol: aSymbol)
]

{ #category : #testing }
ASZigFileContext >> printable: aSymbol [

	^ symbols printable: aSymbol
]
