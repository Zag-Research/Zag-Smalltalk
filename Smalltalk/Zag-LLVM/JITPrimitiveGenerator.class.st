"
I am a connector to the Zag runtime code.

The instance variables are referenced by Zig code in the LLVM module, so must not be re-ordered
"
Class {
	#name : 'JITPrimitiveGenerator',
	#superclass : 'Object',
	#instVars : [
		'builder',
		'module',
		'context'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'API' }
JITPrimitiveGenerator >> add: aNumber to: selfValue [

	<primitive: 'add:to:' module: 'llvm'> 
	Transcript show: 'LLVMBuildAdd, ('
	<< 'aNumber: ' << aNumber << ' , ' 
	<< 'selfValue; ' << selfValue << ')'; cr.
]

{ #category : 'API' }
JITPrimitiveGenerator >> and: valueRef1 with: valueRef2 [
	
	<primitive: 'and:with:' module: 'llvm'> 
	Transcript show: 'LLVMBuildAnd, ('
	<< 'valueRef1: ' << valueRef1 << ' , '
	<< 'valueRef2: ' << valueRef2 << ')'; cr. 
	
	 
]

{ #category : 'API' }
JITPrimitiveGenerator >> compare: valueRef1 is: comparison to: valueRef2 [

	<primitive: 'compare:is:to:' module: 'llvm'>
	Transcript show: 'LLVMBuildICmp, ('
	<< 'valueRef1: ' <<  valueRef1 << ' , ' 
	<< 'comparison: ' << comparison << ' , ' 
	<< 'valueRef2: ' << valueRef2 << ')'; cr.
]

{ #category : 'initialization' }
JITPrimitiveGenerator >> initialize [

	super initialize.
	builder := self makeBuilder.
	module := self makeModule.
	context := self makeContext
]

{ #category : 'initialization' }
JITPrimitiveGenerator >> makeBuilder [

		<primitive: 'makeBuilder' module: 'llvm'> 
]

{ #category : 'initialization' }
JITPrimitiveGenerator >> makeContext [

		<primitive: 'makeContext' module: 'llvm'> 
]

{ #category : 'initialization' }
JITPrimitiveGenerator >> makeModule [

		<primitive: 'makeModule' module: 'llvm'> 
]

{ #category : 'API' }
JITPrimitiveGenerator >> newLabel [

	<primitive: 'newLabel' module: 'llvm'>
	"Creating a new label is like creating a new basic block (BB)"
	"Future note: we must position the builder at the end of the new BB"
	Transcript show: 'LLVMAppendBasicBlock, ()'; cr. 
]

{ #category : 'API' }
JITPrimitiveGenerator >> or: int1 with: int2 [
	
	<primitive: 'or:with:' module: 'llvm'> 
	Transcript show: 'LLVMBuildOr, ('
	<< 'int1: ' << int1 << ' , ' 
	<< 'int2: ' << int2 << ')'; cr.
]

{ #category : 'as yet unclassified' }
JITPrimitiveGenerator >> register: oldReg plus: offset asName: newName [

	<primitive: 'register:plus:asName:' module: 'llvm'>
	"Different than add:to: because we want to move the register by 
	the size of the input it currently points to (e.g. a TagObject)"
	Transcript
	<< 'LLVMBuildGEP2, ('
	<< 'oldReg: ' <<  oldReg << ' , ' 
	<< 'plus: ' << offset << ' , ' 
	<< 'newName: ' << newName << ')'; cr.
]

{ #category : 'API' }
JITPrimitiveGenerator >> returnWith: addressOfValue [
	
	<primitive: 'returnWith:' module: 'llvm'> 
	Transcript show: 'LLVMBuildRet, ('
	<< 'addressOfValue: ' << addressOfValue << ')'; cr. 
	
]

{ #category : 'API' }
JITPrimitiveGenerator >> shiftLeftArithmeticly: num1 by: num2 [
	
	<primitive: 'shiftLeftArithmeticly:by:' module: 'llvm'> 
	Transcript show: 'LLVMBuildShl, ('
	<< 'num1: ' << num1 << ' , '
	<< 'num2: ' << num2 << ')'; cr. 
	
]

{ #category : 'API' }
JITPrimitiveGenerator >> shiftRightArithmeticly: num1 by: num2 [
	
	<primitive: 'shiftLeftArithmeticly:by:' module: 'llvm'> 
	Transcript show: 'LLVMBuildAShr, ('
	<< 'num1: ' << num1 << ' , '
	<< 'num2: ' << num2 << ')'; cr. 
	
]

{ #category : 'API' }
JITPrimitiveGenerator >> store: result at: addressOfSelf [ 

	<primitive: 'store:at:' module: 'llvm'>
	Transcript 
	<< 'LLVMBuildStore, ('
	<< 'result: ' << result << ' , '
	<< 'addressOfSelf: ' << addressOfSelf << ')'; cr. 
]

{ #category : 'API' }
JITPrimitiveGenerator >> valueAt: aString [ 

	<primitive: 'valueAt:' module: 'llvm'>
	Transcript show: 'LLVMBuildLoad2, ('
	<< 'valueAt: ' << aString << ')'; cr.
	 
]
