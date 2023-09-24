"
I have subclasses for all the statments that can exist in the body of a method or a block.
"
Class {
	#name : #ASStatement,
	#superclass : #Object,
	#instVars : [
		'label'
	],
	#classInstVars : [
		'index'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #accessing }
ASStatement class >> nextIndex [
	index ifNil: [ index:=0 ].
	index := index+1.
	^index asString
]

{ #category : #converting }
ASStatement >> asZagLabel [
	label ifNil: [ label:= self className, ASStatement nextIndex].
	^ label
]

{ #category : #accessing }
ASStatement >> blocks [

	^ OrderedCollection new:0
]

{ #category : #testing }
ASStatement >> isASAssign [

	^ false
]

{ #category : #testing }
ASStatement >> isASBlock [
	^false
]

{ #category : #testing }
ASStatement >> isASLiteral [
	^false
]

{ #category : #testing }
ASStatement >> isASRef [
	^false
]

{ #category : #testing }
ASStatement >> isASSelf [
	^false
]

{ #category : #testing }
ASStatement >> isASSend [
	^false
]

{ #category : #testing }
ASStatement >> isASVariable [
	^false
]

{ #category : #testing }
ASStatement >> isReturn [
	^false
]

{ #category : #accessing }
ASStatement >> maxDepth [
	^0
]
