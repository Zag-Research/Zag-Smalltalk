Extension { #name : 'CompiledMethod' }

{ #category : '*Zag-Inspector' }
CompiledMethod >> inspectionZagCode: aBuilder [

	<inspectorPresentationOrder: 60 title: 'ZCode'>
	| ast cg |
	ast := ASPharoCompiler new astFor: self.
	cg := ASCodeGenerator new
		      targetClass: self methodClass;
		      generateFrom: ast.
	cg inline: true.
	cg finalAnalysis.
	
^ (aBuilder instantiate: SpTreeTablePresenter)
		  roots:  cg inspectBasicBlocks;
		  children: [ :aNode | aNode children ];
		  addColumn:
			  (SpStringTableColumn evaluated: #contents);
		  yourself
]

{ #category : '*Zag-Inspector' }
CompiledMethod >> inspectionZigAST: aBuilder [
	<inspectorPresentationOrder: 61 title: 'ZAST'>
	^ (ASPharoCompiler new astFor: self) inspectAST: aBuilder 
]
