"
I will take a class in the current image and compile it to methods of ASTs
"
Class {
	#name : #ASPharoCompiler,
	#superclass : #ASCompiler,
	#instVars : [
		'syms',
		'method',
		'contextClass',
		'codeGenerator'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #'as yet unclassified' }
ASPharoCompiler class >> buildMethod: aCompiledMethod  forClass: aClass withCodeGenerator: anASZigFileGenerator [ 
	^ self new method: aCompiledMethod contextClass: aClass codeGenerator: anASZigFileGenerator
]

{ #category : #method }
ASPharoCompiler >> astFor: aCompiledMethod [

	^ aCompiledMethod astWith: self
]

{ #category : #building }
ASPharoCompiler >> buildFromClass: aClass method: aCompiledMethod [
	method := aCompiledMethod.
	contextClass := aClass.
	methods add: (self compileMethod: method).
]

{ #category : #compiling }
ASPharoCompiler >> compile [

	| visitor anASMethodNode |
	visitor := ASPharoVisitor compiler: self.
	anASMethodNode := visitor visit: method ast.
	
	"result is an AS version of the pharo ast, should inlining occur here?"
	"Now we need to emit the code using a visitor or in the future IBurg implmentation that is rule based"
	
	(ASMethodStatementVisitor codeGenerator: codeGenerator) emit: anASMethodNode selectors: selectors.
	"^ (ASPharoVisitor compiler: self) visit: aCompiledMethod ast"
]

{ #category : #compiling }
ASPharoCompiler >> compileMethod: aCompiledMethod [

	| visitor result |
	visitor := ASPharoVisitor compiler: self.
	result := visitor visit: aCompiledMethod ast.
	^ result
	"^ (ASPharoVisitor compiler: self) visit: aCompiledMethod ast"
]

{ #category : #accessing }
ASPharoCompiler >> dontVisitMessages [
	visitMessages := false
]

{ #category : #'as yet unclassified' }
ASPharoCompiler >> method: aCompiledMethod contextClass: aClass codeGenerator: aCodeGenerator [ 
	method := aCompiledMethod.
	contextClass := aClass.
	codeGenerator := aCodeGenerator 
]
