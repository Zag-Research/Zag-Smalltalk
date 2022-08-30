Extension { #name : #CompiledMethod }

{ #category : #'*ASTSmalltalk' }
CompiledMethod >> astFields [
	self halt.
	^ {  }
]

{ #category : #'*ASTSmalltalk' }
CompiledMethod >> inspectionThread [

	<inspectorPresentationOrder: 50 title: 'Thread'>
	^ self ir inspectionThread
]
