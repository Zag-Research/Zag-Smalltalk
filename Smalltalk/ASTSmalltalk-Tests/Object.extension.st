Extension { #name : #Object }

{ #category : #'*ASTSmalltalk-Tests' }
Object >> zigCGString [
		| file codeGen |
	file := ASZigFileContext new.
	codeGen := ASZigCompileObjectGenerator new context: file.
	self asZig: codeGen.
	^ codeGen asString
]

{ #category : #'*ASTSmalltalk-Tests' }
Object >> zigSLString [

	| file codeGen |
	file := ASZigFileContext new.
	codeGen := ASZigCompileObjectGenerator new context: file.
	self asZig: codeGen.
	^ codeGen setLiteralString
]
