Extension { #name : 'Object' }

{ #category : '*ASTSmalltalk-Tests' }
Object >> processPrimitiveResult: aSemanticInterpreter [

	^ aSemanticInterpreter primitiveSuccess: self
]

{ #category : '*ASTSmalltalk-Tests' }
Object >> zigCGString [

	| file codeGen |
	file := ASZigFileContext new.
	codeGen := ASZigCompileObjectOutput testing context: file.
	self zigCompileObject: codeGen.
	^ codeGen asString
]

{ #category : '*ASTSmalltalk-Tests' }
Object >> zigSLString [

	| file codeGen |
	file := ASZigFileContext new.
	codeGen := ASZigCompileObjectOutput new context: file.
	self zigCompileObject: codeGen.
	^ codeGen setLiteralString 
]
