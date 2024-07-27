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
CompiledMethod >> astImagePut: anASImage [ 
	^ anASImage objectMethod: self
]

{ #category : '*ASTSmalltalk' }
CompiledMethod >> astWith: anASPharoCompiler [ 
	^ anASPharoCompiler compileMethod: self
]

{ #category : '*ASTSmalltalk' }
CompiledMethod class >> canonicalArgumentName [

	^ 'aCompileMethod'
]

{ #category : '*ASTSmalltalk' }
CompiledMethod >> inspectionZagCode [

	<inspectorPresentationOrder: 60 title: 'ZCode'>
	| ast file output |
	ast := ASPharoCompiler new astFor: self.

	file := ASZigTestContext new.
	output := ASZigThreadedOutput testing
		          context: file;
		          generateCode: ast.
	^ output codeGenerator inspectBasicBlocks
]

{ #category : '*ASTSmalltalk' }
CompiledMethod >> inspectionZagThreadedCode [

	<inspectorPresentationOrder: 63 title: 'ZTCode'>
	| ast file output |
	ast := ASPharoCompiler new astFor: self.

	file := ASZigTestContext new.
	output := ASZigThreadedOutput new
		          context: file;
		          generateCode: ast for: self classBinding.
	^ SpTextPresenter new
		  text: file asString;
		  yourself
]

{ #category : '*ASTSmalltalk' }
CompiledMethod >> inspectionZigAST [
	<inspectorPresentationOrder: 61 title: 'ZAST'>
	^ (ASPharoCompiler new astFor: self) inspectAST
]
