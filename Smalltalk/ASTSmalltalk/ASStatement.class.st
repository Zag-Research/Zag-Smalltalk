"
I have subclasses for all the statments that can exist in the body of a method or a block.
"
Class {
	#name : 'ASStatement',
	#superclass : 'Object',
	#classInstVars : [
		'index'
	],
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'accessing' }
ASStatement class >> nextIndex [
	index ifNil: [ index:=0 ].
	index := index+1.
	^index asString
]

{ #category : 'accessing' }
ASStatement class >> resetIndex [
index:= nil
]

{ #category : 'accessing' }
ASStatement >> blocks [

	^ OrderedCollection new:0
]

{ #category : 'accessing' }
ASStatement >> children [

	^ {  }
]

{ #category : 'testing' }
ASStatement >> inferType: aGenerator [
	^ nil
]

{ #category : 'compiling' }
ASStatement >> invalidAST [

	^ ASTInvalid signal
]

{ #category : 'compiling' }
ASStatement >> invalidAST: aString [

	^ ASTInvalid signal: aString
]

{ #category : 'testing' }
ASStatement >> isASAssign [

	^ false
]

{ #category : 'testing' }
ASStatement >> isASBlock [
	^false
]

{ #category : 'testing' }
ASStatement >> isASLiteral [
	^false
]

{ #category : 'testing' }
ASStatement >> isASRef [
	^false
]

{ #category : 'testing' }
ASStatement >> isASSelf [
	^false
]

{ #category : 'testing' }
ASStatement >> isASSend [
	^false
]

{ #category : 'testing' }
ASStatement >> isASVariable [
	^false
]

{ #category : 'testing' }
ASStatement >> isImmediateThunk [
^ false
]

{ #category : 'testing' }
ASStatement >> isReturn [
	^false
]

{ #category : 'accessing' }
ASStatement >> maxDepth [
	^0
]

{ #category : 'compiling' }
ASStatement >> zigWalk: aGenerator [

	self subclassResponsibility 
]

{ #category : 'compiling' }
ASStatement >> zigWalkBlockReturn: aGenerator [

	self subclassResponsibility
]

{ #category : 'compiling' }
ASStatement >> zigWalkDiscard: aGenerator [

	self subclassResponsibility
]
