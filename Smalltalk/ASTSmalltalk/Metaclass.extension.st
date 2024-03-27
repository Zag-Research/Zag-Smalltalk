Extension { #name : #Metaclass }

{ #category : #'*ASTSmalltalk' }
Metaclass >> astFields [
	^ { thisClass . methodDict values . superclass }
]
