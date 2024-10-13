"
I am an abstract class for all the compilable unitss that are dispatchable as opposed to inlined.
"
Class {
	#name : 'ASCDispatchableBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#traits : 'ASCCompiledBlock',
	#classTraits : 'ASCCompiledBlock classTrait',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'categories' }
ASCDispatchableBlock class >> recategorizeSelector: selector from: oldProtocol to: newProtocol [ 
	^ self subclassResponsibility
]
