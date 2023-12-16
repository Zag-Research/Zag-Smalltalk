Extension { #name : 'Set' }

{ #category : '*ASTSmalltalk' }
Set >> addIfNotPresent: anObject andDo: aBlock [
	"Include anObject as one of the receiver's elements and then value aBlock, but only if there
	is no such element already. Anwser anObject."

	| index |
	index := self scanFor: anObject.
	(array at: index) 
		ifNil: [self atNewIndex: index put: anObject asCollectionElement. aBlock value ].
	^ anObject

]
