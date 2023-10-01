Extension { #name : #CompiledMethod }

{ #category : #'*ASTSmalltalk' }
CompiledMethod >> asAST [
	^ ASTIrMethod new from: self
]

{ #category : #'*ASTSmalltalk' }
CompiledMethod >> astFields [
	self halt.
	^ {  }
]

{ #category : #'*ASTSmalltalk' }
CompiledMethod >> astWith: anASPharoCompiler [ 
	^ anASPharoCompiler compileMethod: self
]

{ #category : #'*ASTSmalltalk' }
CompiledMethod >> inspectionThread [

	<inspectorPresentationOrder: 50 title: 'Thread'>
	^ self ir inspectionThread
]
