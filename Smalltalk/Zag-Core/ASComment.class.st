"
I represent a comment in code.  My #text variable contains the text of the comment, excluding the surrounding double quotes.  My #position variable is optional and denotes where the comment is in code.  The value #inline menas it is on the same line as the part of the code before it, while #separate means it is on its own line.  More values may be added in the future.
"
Class {
	#name : 'ASComment',
	#superclass : 'AST',
	#instVars : [
		'text',
		'position'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'accessing' }
ASComment class >> text: aString [
	"Creates an ASComment with the provided text."
	^self new text: aString; yourself
]

{ #category : 'accessing' }
ASComment class >> text: aString position: aSymbol [
	"Creates an ASComment with the provided text."
	^self new text: aString; position: aSymbol; yourself
]

{ #category : 'comparing' }
ASComment >> = other [
	^self class = other class and: [ self text = other text ]
]

{ #category : 'visiting' }
ASComment >> accept: aVisitor [
	^aVisitor visitComment: self
]

{ #category : 'converting' }
ASComment >> asPharoAST [
	"Converts me to the associated Pharo AST."
	^OCCommentNode new
		with: self text from: nil to: nil;
		yourself
]

{ #category : 'comparing' }
ASComment >> hash [
	^(super hash hashMultiply + self text hash) hashMultiply
]

{ #category : 'accessing' }
ASComment >> position [
	^position
]

{ #category : 'setting' }
ASComment >> position: aSymbol [
	position := aSymbol
]

{ #category : 'accessing' }
ASComment >> text [
	^text
]

{ #category : 'setting' }
ASComment >> text: aString [
	text := aString
]
