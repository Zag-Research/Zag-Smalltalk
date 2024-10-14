Extension { #name : 'ASStack' }

{ #category : '*ASTSmalltalk-Tests' }
ASStack >> pushForTests: anObject [

	^ (ASStackTemporary fromStack: self) data: (AStackValue fromValue: anObject)
]
