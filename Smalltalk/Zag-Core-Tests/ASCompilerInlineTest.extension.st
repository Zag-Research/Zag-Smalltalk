Extension { #name : 'ASCompilerInlineTest' }

{ #category : '*Zag-Core-Tests' }
ASCompilerInlineTest >> testRecursiveSum [
	"recursiveSum: listOfNumbers
		listOfNumbers isEmpty ifTrue: [^0].
		^ listOfNumbers first + listOfNumbers allButFirst"

	self
		assertEquivalentSending: #recursiveSum:
		arguments: #( #( 4 5 6 ) )
		to: ASCompileTestClass1 new
]
