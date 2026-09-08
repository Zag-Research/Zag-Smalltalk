"
I represent a comment in code.  My #text variable contains the text of the comment, excluding the surrounding double quotes.
"
Class {
	#name : 'ASComment',
	#superclass : 'AST',
	#instVars : [
		'text'
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

{ #category : 'accessing' }
ASComment >> text [
	^text
]

{ #category : 'setting' }
ASComment >> text: aString [
	text := aString
]
