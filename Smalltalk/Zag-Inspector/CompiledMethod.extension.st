Extension { #name : 'CompiledMethod' }

{ #category : '*Zag-Inspector' }
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

{ #category : '*Zag-Inspector' }
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

{ #category : '*Zag-Inspector' }
CompiledMethod >> inspectionZigAST [
	<inspectorPresentationOrder: 61 title: 'ZAST'>
	^ (ASPharoCompiler new astFor: self) inspectAST
]
