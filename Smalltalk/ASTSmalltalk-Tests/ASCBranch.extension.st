Extension { #name : 'ASCBranch' }

{ #category : '*ASTSmalltalk-Tests' }
ASCBranch >> interpretFor: aSemanticInterpreter [

	^ aSemanticInterpreter branch: bb
]
