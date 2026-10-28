Extension { #name : 'ASCReturnBlock' }

{ #category : '*Zag-Core-Tests' }
ASCReturnBlock >> returnTopNonLocal [
	"want to return the top element on the stack.
	 - need to temp save the top
	 - need to update sp to the return of the send in the method that defined the block
			- this sp should have been saved in the block context associated with this context
	 - need to save the temp value at the new sp location
	 - need to update the pc to the context methods saved pc
	"
	^ self addOperation: (ASCReturnNonLocal new)
]
