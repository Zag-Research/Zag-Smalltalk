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

{ #category : #'*ASTSmalltalk' }
CompiledMethod >> inspectionZigAST [
	<inspectorPresentationOrder: 61 title: 'ZagAST'>
	^ (ASPharoCompiler new astFor: self) inspectAST
]

{ #category : #'*ASTSmalltalk' }
CompiledMethod >> inspectionZigBlocks [
	<inspectorPresentationOrder: 60 title: 'ZigBlocks'>
	| ast file output |
	ast := ASPharoCompiler new astFor: self.

	file := ASZigTestContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCode: ast.

	^ output codeGenerator inspectBasicBlocks
]
