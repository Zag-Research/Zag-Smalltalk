Extension { #name : #Class }

{ #category : #'*ASTSmalltalk' }
Class >> astFields [
	^ { name asSymbol . methodDict values . superclass . Array new . 1 . classPool keys . sharedPools }
]
