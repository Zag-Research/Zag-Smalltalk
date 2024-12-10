Extension { #name : 'ASStack' }

{ #category : '*Zag-Core-Tests' }
ASStack >> pushForTests: anObject [

	^ (ASStackTemporary fromStack: self) data: (AStackValue fromValue: anObject)
]
