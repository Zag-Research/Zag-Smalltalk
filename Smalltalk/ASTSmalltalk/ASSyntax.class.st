Class {
	#name : #ASSyntax,
	#superclass : #ASToken,
	#classInstVars : [
		'classMap'
	],
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #'instance creation' }
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
			$| -> ASBarToken .
			$; -> ASCascadeToken .
			':=' -> ASAssignmentToken .
			':>' -> ASParrotToken .
		} asDictionary
]

{ #category : #'instance creation' }
ASSyntax class >> with: aCharacterOrString [ 
	^ (classMap at: aCharacterOrString) new value: aCharacterOrString
]
