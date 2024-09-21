Extension { #name : 'Set' }

{ #category : '*ASTSmalltalk' }
Set >> add: anObject andIfNewDo: aBlock [
	"Include anObject as one of the receiver's elements and then value aBlock, but only if there
	is no such element already. Anwser anObject."

	| index |
	index := self scanFor: anObject.
	(array at: index) 
		ifNil: [self atNewIndex: index put: anObject asCollectionElement. aBlock value ].
	^ anObject

]

{ #category : '*ASTSmalltalk' }
Set >> add: anObject andIfNewDo: newBlock orIfExistedDo: existedBlock [
	"Include anObject as one of the receiver's elements and then value newBlock, but only if there
	is no such element already. If there is, value existedBlock. Anwser anObject."

	| index |
	index := self scanFor: anObject.
	(array at: index) ifNil: [
		self atNewIndex: index put: anObject asCollectionElement.
		newBlock value ] ifNotNil: [ existedBlock value ].
	^ anObject
]
