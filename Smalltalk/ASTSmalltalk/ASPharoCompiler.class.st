"
I will take a class in the current image and compile it to methods of ASTs
"
Class {
	#name : #ASPharoCompiler,
	#superclass : #ASCompiler,
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #compiling }
ASPharoCompiler >> compileMethod: aCompiledMethod [ 
	^ (ASPharoVisitor compiler: self) visit: aCompiledMethod ast
]
