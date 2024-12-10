Class {
	#name : 'ASSyntax',
	#superclass : 'ASToken',
	#classInstVars : [
		'classMap'
	],
	#category : 'Zag-Core-Tokens',
	#package : 'Zag-Core',
	#tag : 'Tokens'
}

{ #category : 'class initialization' }
ASSyntax class >> initialize [
	classMap := {
			$^ -> ASReturnToken .
			$: -> ASColonToken .
			$. -> ASSeparatorToken .
			$[ -> ASBlockStartToken .
			$] -> ASBlockEndToken .
			${ -> ASArrayBuildToken .
			$} -> ASArrayEndToken .
			$( -> ASOpenParenToken .
			$) -> ASCloseParenToken .
			'#(' -> ASLiteralListToken .
			$| -> ASBarToken .
			'|' -> ASBarToken .
			$; -> ASCascadeToken .
			':=' -> ASAssignmentToken .
			':>' -> ASParrotToken .
		} asDictionary
]

{ #category : 'instance creation' }
ASSyntax class >> with: aCharacterOrString [ 
	^ (classMap at: aCharacterOrString ifAbsent: [^ self error: 'invalid character sequence: ',aCharacterOrString ]) new value: aCharacterOrString
]
