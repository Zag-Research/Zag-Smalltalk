"
I represent a return
"
Class {
	#name : 'ASCReturnTop',
	#superclass : 'ASCReturn',
	#instVars : [
		'stackElement'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'callgraph' }
ASCReturnTop >> dotEdgesOn: aWriteStream from: aBasicBlock [
]

{ #category : 'callgraph' }
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

{ #category : 'code generation' }
ASCReturnTop >> outputTo: output [

	^ output visitReturnTop: self
]

{ #category : 'stack' }
ASCReturnTop >> resultStack: aStack [

	stack := aStack.
	stackElement := aStack at: #self.
	^ self
		  consumes: aStack;
		  createsTemporary: stackElement next data: aStack data
]
