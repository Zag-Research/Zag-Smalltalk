"
I represent an argument to a method or a block
"
Class {
	#name : #ASArg,
	#superclass : #ASVariable,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #visiting }
ASArg >> acceptVisitor: aVisitor [ 
	^ aVisitor visitArg: self
]

{ #category : #building }
ASArg >> zigString: syms and: stackVariables [
	"The argument is on the stack already so should not need to push it"

	^ '&e.pushLocal, ' , ((stackVariables indexOf: name) - 1) asString
	  , ', '
]
