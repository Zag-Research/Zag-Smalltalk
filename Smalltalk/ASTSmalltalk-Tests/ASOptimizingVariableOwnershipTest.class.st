"
I test algorithm that optimizes the ownership of variables in blocks. See https://github.com/Zag-Research/Zag-Smalltalk/edit/main/Documentation/Final%20Analysis.md#optimizing-local-variable-locations for more information.
"
Class {
	#name : 'ASOptimizingVariableOwnershipTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testBlockVariable [
"If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created)."

| method |
method := ASCCompiledMethodBlock new.
method declares add: #myVar.
method modified add: #myVar.

method deriveOwnership.

self assert: (method owns isNotEmpty)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testSimpleOneBlockDeclares [
"If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created)."

| method |
method := ASCCompiledMethodBlock new.
method declares add: #myVar.

method deriveOwnership.

self assert: (method owns isEmpty)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testSimpleOneBlockDeclaresAndModifies [
"If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created)."

| method |
method := ASCCompiledMethodBlock new.
method declares add: #myVar.
method modified add: #myVar.

method deriveOwnership.

self assert: (method owns includes: #myVar)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testSimpleOneBlockDeclaresAndReferences [
"If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created)."

| method |
method := ASCCompiledMethodBlock new.
method declares add: #myVar.
method referenced add: #myVar.

method deriveOwnership.

self assert: (method owns includes: #myVar) 
]
