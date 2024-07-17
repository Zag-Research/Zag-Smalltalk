"
I will take a class in the current image and compile it to methods of ASTs
"
Class {
	#name : 'ASPharoCompiler',
	#superclass : 'ASCompiler',
	#instVars : [
		'syms',
		'method',
		'contextClass',
		'codeGenerator'
	],
	#category : 'ASTSmalltalk-Compiler',
	#package : 'ASTSmalltalk',
	#tag : 'Compiler'
}

{ #category : 'as yet unclassified' }
ASPharoCompiler class >> buildMethod: aCompiledMethod  forClass: aClass withCodeGenerator: anASZigFileGenerator [ 
	^ self new method: aCompiledMethod contextClass: aClass codeGenerator: anASZigFileGenerator
]

{ #category : 'method' }
ASPharoCompiler >> astFor: aCompiledMethod [

	^ aCompiledMethod astWith: self
]

{ #category : 'building' }
ASPharoCompiler >> buildFromClass: aClass method: aCompiledMethod [
	method := aCompiledMethod.
	contextClass := aClass.
	methods add: (self compileMethod: method).
]

{ #category : 'compiling' }
ASPharoCompiler >> compileMethod: aCompiledMethod [

	| visitor result |
	visitor := ASPharoVisitor compiler: self.
	result := visitor visit: aCompiledMethod ast.
	result compiledMethod: aCompiledMethod.
	^ result
	"^ (ASPharoVisitor compiler: self) visit: aCompiledMethod ast"
]

{ #category : 'accessing' }
ASPharoCompiler >> dontVisitMessages [
	visitMessages := false
]

{ #category : 'as yet unclassified' }
ASPharoCompiler >> method: aCompiledMethod contextClass: aClass codeGenerator: aCodeGenerator [ 
	method := aCompiledMethod.
	contextClass := aClass.
	codeGenerator := aCodeGenerator 
]
