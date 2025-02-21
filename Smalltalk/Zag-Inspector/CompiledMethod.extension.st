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
CompiledMethod >> inspectionZigAST [
	<inspectorPresentationOrder: 61 title: 'ZAST'>
	^ (ASPharoCompiler new astFor: self) inspectAST
]
