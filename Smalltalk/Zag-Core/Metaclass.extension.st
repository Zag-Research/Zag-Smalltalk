Extension { #name : 'Metaclass' }

{ #category : '*Zag-Core' }
Metaclass >> astFields [
	^ { thisClass . methodDict values . superclass }
]
