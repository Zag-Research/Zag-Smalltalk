Extension { #name : 'ASFlowPhi' }

{ #category : '*ASTSmalltalk-Tests' }
ASFlowPhi >> asAssociationList [ 
	
	| associationList |
	associationList := OrderedCollection new.
	sources do: [ :source | associationList add: (source basicBlock -> source)].
	^ associationList.
]
