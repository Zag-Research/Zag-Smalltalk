"
I am a Phi node, which means I get values from other blocks.
If all predecessor blocks have the same value, I will represent that.
If any are different, then I will generate a Phi node for LLVM to allocate.
"
Class {
	#name : 'ASFlowPhi',
	#superclass : 'ASFlowNonNil',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'testing' }
ASFlowPhi >> isPhiReference [

	^ true
]
