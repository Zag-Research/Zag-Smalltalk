Extension { #name : 'CompiledMethod' }

{ #category : '*Zag-Core' }
CompiledMethod >> astFields [
	self halt.
	^ {  }
]

{ #category : '*Zag-Core' }
CompiledMethod >> astImageObject: anASImage [

	^ anASImage objectMethod: self
]

{ #category : '*Zag-Core' }
CompiledMethod >> astWith: anASPharoCompiler [ 
	^ anASPharoCompiler compileMethod: self
]

{ #category : '*Zag-Core' }
CompiledMethod class >> canonicalArgumentName [

	^ 'aCompileMethod'
]

{ #category : '*Zag-Core' }
CompiledMethod >> inspectionZagCode [

	<inspectorPresentationOrder: 60 title: 'ZCode'>
	| ast cg |
	ast := ASPharoCompiler new astFor: self.
	cg := ASCodeGenerator new
		      targetClass: self methodClass;
		      generateFrom: ast.
	cg inline: true.
	cg finalAnalysis.
	^ cg inspectBasicBlocks
]

{ #category : '*Zag-Core' }
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

{ #category : '*Zag-Core' }
CompiledMethod >> inspectionZigAST [
	<inspectorPresentationOrder: 61 title: 'ZAST'>
	^ (ASPharoCompiler new astFor: self) inspectAST
]
