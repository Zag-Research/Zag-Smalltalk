Class {
	#name : 'ASLLVMTests',
	#superclass : 'ASCodeTests',
	#instVars : [
		'file'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASLLVMTests >> setUp [

	super setUp.

	"Convert to LLVM specific context"
	file := ASLLFileContext new
]
