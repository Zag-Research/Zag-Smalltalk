"
I am a connector to the Zag runtime code.

The instance variables are referenced by Zig code in the LLVM module, so must not be re-ordered
"
Class {
	#name : 'ZagLLVMGenerator',
	#superclass : 'ZagLLVMGeneratorAPI',
	#instVars : [
		'builder',
		'module',
		'context'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'instance creation' }
ZagLLVMGenerator class >> new [

	<primitive: 'createBuilderObject' module: 'llvm'>
	Transcript show: 'BuilderInit'; cr.
]

{ #category : 'API' }
ZagLLVMGenerator >> add: aNumber to: selfValue [

	<primitive: 'add:to:' module: 'llvm'> 
	Transcript show: 'LLVMBuildAdd, ('
	<< 'aNumber: ' << aNumber << ' , ' 
	<< 'selfValue; ' << selfValue << ')'; cr.
]

{ #category : 'API' }
ZagLLVMGenerator >> and: valueRef1 with: valueRef2 [
	
	<primitive: 'and:with:' module: 'llvm'> 
	Transcript show: 'LLVMBuildAnd, ('
	<< 'valueRef1: ' << valueRef1 << ' , '
	<< 'valueRef2: ' << valueRef2 << ')'; cr. 
	
	 
]

{ #category : 'API' }
ZagLLVMGenerator >> compare: valueRef1 is: comparison to: valueRef2 [

	<primitive: 'compare:is:to:' module: 'llvm'>
	Transcript show: 'LLVMBuildICmp, ('
	<< 'valueRef1: ' <<  valueRef1 << ' , ' 
	<< 'comparison: ' << comparison << ' , ' 
	<< 'valueRef2: ' << valueRef2 << ')'; cr.
]

{ #category : 'initialization' }
ZagLLVMGenerator >> initialize [

	super initialize.
	builder := self makeBuilder.
	module := self makeModule.
	context := self makeContext
]

{ #category : 'initialization' }
ZagLLVMGenerator >> makeBuilder [

		<primitive: 'makeBuilder' module: 'llvm'> 
]

{ #category : 'initialization' }
ZagLLVMGenerator >> makeContext [

		<primitive: 'makeContext' module: 'llvm'> 
]

{ #category : 'initialization' }
ZagLLVMGenerator >> makeModule [

		<primitive: 'makeModule' module: 'llvm'> 
]

{ #category : 'API' }
ZagLLVMGenerator >> newLabel [

	<primitive: 'newLabel' module: 'llvm'>
	"Creating a new label is like creating a new basic block (BB)"
	"Future note: we must position the builder at the end of the new BB"
	Transcript show: 'LLVMAppendBasicBlock, ()'; cr. 
]

{ #category : 'API' }
ZagLLVMGenerator >> or: int1 with: int2 [
	
	<primitive: 'or:with:' module: 'llvm'> 
	Transcript show: 'LLVMBuildOr, ('
	<< 'int1: ' << int1 << ' , ' 
	<< 'int2: ' << int2 << ')'; cr.
]

{ #category : 'as yet unclassified' }
ZagLLVMGenerator >> register: oldReg plus: offset asName: newName [

	<primitive: 'register:plus:asName:' module: 'llvm'>
	"Different than add:to: because we want to move the register by 
	the size of the input it currently points to (e.g. a TagObject)"
	Transcript show: 'LLVMBuildGEP2, ('
	<< 'oldReg: ' <<  oldReg << ' , ' 
	<< 'plus: ' << offset << ' , ' 
	<< 'newName: ' << newName << ')'; cr.
]

{ #category : 'API' }
ZagLLVMGenerator >> returnWith: addressOfValue [
	
	<primitive: 'returnWith:' module: 'llvm'> 
	Transcript show: 'LLVMBuildRet, ('
	<< 'addressOfValue: ' << addressOfValue << ')'; cr. 
	
]

{ #category : 'API' }
ZagLLVMGenerator >> shiftLeftArithmeticly: num1 by: num2 [
	
	<primitive: 'shiftLeftArithmeticly:by:' module: 'llvm'> 
	Transcript show: 'LLVMBuildShl, ('
	<< 'num1: ' << num1 << ' , '
	<< 'num2: ' << num2 << ')'; cr. 
	
]

{ #category : 'API' }
ZagLLVMGenerator >> shiftRightArithmeticly: num1 by: num2 [
	
	<primitive: 'shiftLeftArithmeticly:by:' module: 'llvm'> 
	Transcript show: 'LLVMBuildAShr, ('
	<< 'num1: ' << num1 << ' , '
	<< 'num2: ' << num2 << ')'; cr. 
	
]

{ #category : 'API' }
ZagLLVMGenerator >> store: result at: addressOfSelf [ 

	<primitive: 'store:at:' module: 'llvm'>
	Transcript show: 'LLVMBuildStore, ('
	<< 'result: ' << result << ' , '
	<< 'addressOfSelf: ' << addressOfSelf << ')'; cr. 
]

{ #category : 'API' }
ZagLLVMGenerator >> valueAt: aString [ 

	<primitive: 'valueAt:' module: 'llvm'>
	Transcript show: 'LLVMBuildLoad2, ('
	<< 'valueAt: ' << aString << ')'; cr.
	 
]
