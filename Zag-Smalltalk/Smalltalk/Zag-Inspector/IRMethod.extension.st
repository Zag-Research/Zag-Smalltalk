Extension { #name : 'IRMethod' }

{ #category : '*Zag-Inspector' }
IRMethod >> inspectionThread [

	<inspectorPresentationOrder: 50 title: 'Thread'>
	^ SpTextPresenter new
		  text: (ASTThreadFormat new ir: self);
		  yourself
]
