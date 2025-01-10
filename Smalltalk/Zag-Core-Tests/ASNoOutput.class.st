"
An outputter that doesn't actually output anything, but simply compiles a method and returns the CodeGenerator for analysis.
"
Class {
	#name : 'ASNoOutput',
	#superclass : 'ASOutput',
	#category : 'Zag-Core-Tests-Tests',
	#package : 'Zag-Core-Tests',
	#tag : 'Tests'
}

{ #category : 'compiling' }
ASNoOutput >> compileMethod: aCompiledMethod [

	^ (self generateCode: (ASPharoCompiler new astFor: aCompiledMethod)
		            for: aCompiledMethod methodClass) codeGenerator

]

{ #category : 'initialization' }
ASNoOutput >> initialize [

	super initialize.
	self context: ASFileContext new
]

{ #category : 'accessing' }
ASNoOutput >> name [

	^ 'none'
]
