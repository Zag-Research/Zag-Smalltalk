Extension { #name : 'Class' }

{ #category : '*Zag-Core' }
Class >> astFields [
	^ { name asSymbol . methodDict values . superclass . Array new . 1 . classPool keys . sharedPools }
]
