Extension { #name : 'ASCSends' }

{ #category : '*Zag-Inlining' }
ASCSends >> inlineIfPossible: anASCodeGenerator [

	(ASInliner lookup: selector stack: stack) ifNotNil: [ : inliner | ^ inliner inline: self with: anASCodeGenerator ]
]
