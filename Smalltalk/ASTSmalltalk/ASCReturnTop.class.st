"
I represent a return
"
Class {
	#name : 'ASCReturnTop',
	#superclass : 'ASCReturn',
	#instVars : [
		'stackElement'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'as yet unclassified' }
ASCReturnTop >> dotEdgesOn: aWriteStream from: aBasicBlock [
]

{ #category : 'API' }
ASCReturnTop >> dotStringOn: aStream [

	^ aStream << 'return top'
]

{ #category : 'output' }
ASCReturnTop >> getTop: anASLLMethodOutput [

	^ stack getNameOn: anASLLMethodOutput 
]

{ #category : 'Collection API' }
ASCReturnTop >> inlineWithFixedCFG: anASCodeGenerator [ 

]

{ #category : 'Collection API' }
ASCReturnTop >> interpretFor: anInterpreter [

	^ anInterpreter return: anInterpreter top selfOffset: stack offset - stackElement offset
]

{ #category : 'block handling' }
ASCReturnTop >> manifestParameters [

	^ stack makeManifestFor: 1
]

{ #category : 'data flow instructions' }
ASCReturnTop >> markReferences [

	stack markReferenced
]

{ #category : 'code generation' }
ASCReturnTop >> outputTo: output [

	^ output visitReturnTop: self
]

{ #category : 'stack' }
ASCReturnTop >> resultStack: aStack basicBlock: aBasicBlock [

	stack := aStack.
	stackElement := aStack at: #self.
	^ (ASStackTemporary fromStack: stackElement next) data: aStack data
]
