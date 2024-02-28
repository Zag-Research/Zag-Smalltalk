"
I am a stream for stdio
"
Class {
	#name : 'ASTStdIOStrream',
	#superclass : 'ASTObject',
	#instVars : [
		'peeked',
		'atEnd'
	],
	#category : 'ASTKernel-Streams',
	#package : 'ASTKernel-Streams'
}

{ #category : 'testing' }
ASTStdIOStrream >> atEnd [

	^ atEnd
]

{ #category : 'initialization' }
ASTStdIOStrream >> initialize [

	super initialize.
	atEnd := false
]

{ #category : 'accessing' }
ASTStdIOStrream >> next [
	peeked ifNotNil: [ : c | peeked := nil. ^ c ].
	^ self unicodeRead ifNil: [ atEnd := true ]
]

{ #category : 'accessing' }
ASTStdIOStrream >> nextPut: aCharacter [
	"Primitive. Output the Character on stderr"

	<primitive: 2002>
]

{ #category : 'accessing' }
ASTStdIOStrream >> nextPutAll: aString [
	"Primitive. Output the String on stderr"

	<primitive: 2003>
	
]

{ #category : 'accessing' }
ASTStdIOStrream >> nextPutAllOnError: aString [
	"Primitive. Output the String on stderr"

	<primitive: 2005>
	
]

{ #category : 'accessing' }
ASTStdIOStrream >> nextPutOnError: aCharacter [
	"Primitive. Output the Character on stderr"

	<primitive: 2004>
]

{ #category : 'accessing' }
ASTStdIOStrream >> peek [

	peeked ifNotNil: [ :c |
		^ c ].
	^ peeked := self unicodeRead ifNil: [ atEnd := true ]
]

{ #category : 'private' }
ASTStdIOStrream >> unicodeRead [

	<primitive: 2001>
	^ self primitiveFailed
]
