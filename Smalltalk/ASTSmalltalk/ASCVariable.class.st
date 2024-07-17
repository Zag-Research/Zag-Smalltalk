"
I represent a variable (local, parameters, inherited local/parameters, instance, class, shared).
"
Class {
	#name : 'ASCVariable',
	#superclass : 'Object',
	#instVars : [
		'name'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'comparing' }
ASCVariable >> = other [

	^ self class = other class and: [ self name = other name ]
]

{ #category : 'printing' }
ASCVariable >> inspectStringOn: s [

	s << self className << ' ' <<
	name 
]

{ #category : 'printing' }
ASCVariable >> inspectZagASTOn: s [

	s << self className << $(.
	s << name.
	s << $)
]

{ #category : 'testing' }
ASCVariable >> isASArg [
	^false
]

{ #category : 'comparing' }
ASCVariable >> isASBlock [

	^ false
]

{ #category : 'comparing' }
ASCVariable >> name [

	^ name
]

{ #category : 'comparing' }
ASCVariable >> name: aSymbol [

	name := aSymbol
]

{ #category : 'printing' }
ASCVariable >> printOn: s [

	s << self className.
	name ifNotNil: [ s << ' ' << name ]
]

{ #category : 'compiling' }
ASCVariable >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sentToUnknownObject: aSymbol
]
