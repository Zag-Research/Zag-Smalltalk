Extension { #name : 'CompiledMethod' }

{ #category : '*ASTSmalltalk' }
CompiledMethod >> asAST [
	^ ASTIrMethod new from: self
]

{ #category : '*ASTSmalltalk' }
CompiledMethod >> astFields [
	self halt.
	^ {  }
]

{ #category : '*ASTSmalltalk' }
CompiledMethod >> astWith: anASPharoCompiler [ 
	^ anASPharoCompiler compileMethod: self
]

{ #category : '*ASTSmalltalk' }
CompiledMethod >> inspectionZagCode [

	<inspectorPresentationOrder: 60 title: 'ZagCode'>
	| ast file output |
	ast := ASPharoCompiler new astFor: self.

	file := ASZigTestContext new.
	output := ASZigThreadedOutput testing
		          context: file;
		          generateCode: ast.

	^ output codeGenerator inspectBasicBlocks
]

{ #category : '*ASTSmalltalk' }
CompiledMethod >> inspectionZigAST [
	<inspectorPresentationOrder: 61 title: 'ZagAST'>
	^ (ASPharoCompiler new astFor: self) inspectAST
]
