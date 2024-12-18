"
I am an error raised if the code generator detects an invalid AST (such as `super` in a non-target position)
"
Class {
	#name : 'ASTInvalid',
	#superclass : 'Error',
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'accessing' }
ASTInvalid >> messageText [

	^ String streamContents: [ :strm |
		strm nextPutAll: 'Invalid AST detected' .
		  messageText ifNotNil: [
			  strm
				nextPutAll: ': ';
				  nextPutAll: messageText ]]
]
