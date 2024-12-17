Extension { #name : 'Object' }

{ #category : '*Zag-Core-Tests' }
Object >> processPrimitiveResult: aSemanticInterpreter [

	^ aSemanticInterpreter primitiveSuccess: self
]

{ #category : '*Zag-Core-Tests' }
Object >> zigCGString [

	| file codeGen |
	file := ASZigFileContext new.
	codeGen := ASZigCompileObjectOutput testing context: file.
	self zigCompileObject: codeGen.
	^ codeGen asString
]

{ #category : '*Zag-Core-Tests' }
Object >> zigSLString [

	| file codeGen |
	file := ASZigFileContext new.
	codeGen := ASZigCompileObjectOutput new context: file.
	self zigCompileObject: codeGen.
	^ codeGen setLiteralString 
]
