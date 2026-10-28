Extension { #name : 'ASCCompiledMethodBlock' }

{ #category : '*Zag-Core-Tests' }
ASCCompiledMethodBlock >> withStack: aStack target: atargetClass [

	stack := aStack initSelf.
	^ stack
		  class: atargetClass;
		  data: (AStackValue fromClass: atargetClass)
]
