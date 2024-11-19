Extension { #name : 'Bag' }

{ #category : '*ASTSmalltalk' }
Bag >> sizeAsSet [
	"Answer the number of elements when thought of as a Set."

	"#(1 2 2 4 1 1 1) asBag sizeAsSet >>> 3"

	^ contents size
]
