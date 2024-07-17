Class {
	#name : 'ASCMethodBlock',
	#superclass : 'ASCBlockClosureBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCMethodBlock >> addSourceBlock: aBasicBlock [
	" nothing to do, because this can only happen with tail recursive calls "

	self halt
]

{ #category : 'writing' }
ASCMethodBlock >> sourceElementsForInspect [

	^ OrderedCollection new addAll: sourceElements replicate;yourself
]
