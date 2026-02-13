Class {
	#name : 'ASSyntax',
	#superclass : 'ASToken',
	#classInstVars : [
		'classMap'
	],
	#category : 'Zag-Compiler-Tokens',
	#package : 'Zag-Compiler',
	#tag : 'Tokens'
}

{ #category : 'class initialization' }
ASSyntax class >> initialize [

	classMap := {
		            ('^' -> ASReturnToken).
		            (':' -> ASColonToken).
		            ('.' -> ASSeparatorToken).
		            ('[' -> ASBlockStartToken).
		            (']' -> ASBlockEndToken).
		            ('{' -> ASArrayBuildToken).
		            ('}' -> ASArrayEndToken).
		            ('(' -> ASOpenParenToken).
		            (')' -> ASCloseParenToken).
		            ('#(' -> ASLiteralListToken).
		            ('|' -> ASBarToken).
		            (';' -> ASCascadeToken).
		            (':=' -> ASAssignmentToken).
		            (':>' -> ASParrotToken) } asDictionary
]

{ #category : 'instance creation' }
ASSyntax class >> with: aCharacterOrString [

	^ self with: aCharacterOrString
		   ifAbsent: [
		   ^ self error: 'invalid character sequence: ' , aCharacterOrString ]
]

{ #category : 'instance creation' }
ASSyntax class >> with: aCharacterOrString ifAbsent: aBlock [

	^ (classMap
		   at: aCharacterOrString
		   ifAbsent: [ aBlock value ])
		  new value: aCharacterOrString
]
