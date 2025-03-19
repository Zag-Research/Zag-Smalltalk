"
I am a connector to the Zag runtime code.

`new` is a primitive that returns an object that connects to a dispatch table that calls the LLVM-Zig interface to LLVM.
"
Class {
	#name : 'ZagLLVMBuilder',
	#superclass : 'ZagLLVMGeneratorAPI',
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'instance creation' }
ZagLLVMBuilder class >> new [

	<primitive: 'createBuilderObject' module: 'llvm'>
]

{ #category : 'API' }
ZagLLVMBuilder >> add: aNumber to: selfValue [

	<primitive: 'add:to:' module: 'llvm'> 
	Transcript show: 'LLVMBuildAdd, ('
	<< 'aNumber: ' << aNumber << ' , ' 
	<< 'selfValue; ' << selfValue << ')'; cr.
]

{ #category : 'API' }
ZagLLVMBuilder >> and: valueRef1 with: valueRef2 [
	
	<primitive: 'and:with:' module: 'llvm'> 
	Transcript show: 'LLVMBuildAnd, ('
	<< 'valueRef1: ' << valueRef1 << ' , '
	<< 'valueRef2: ' << valueRef2 << ')'; cr. 
	
	 
]

{ #category : 'API' }
ZagLLVMBuilder >> compare: valueRef1 is: comparison to: valueRef2 [

	<primitive: 'compare:is:to:' module: 'llvm'>
	Transcript show: 'LLVMBuildICmp, ('
	<< 'valueRef1: ' <<  valueRef1 << ' , ' 
	<< 'comparison: ' << comparison << ' , ' 
	<< 'valueRef2: ' << valueRef2 << ')'; cr.
]

{ #category : 'API' }
ZagLLVMBuilder >> newLabel [

	<primitive: 'newLabel' module: 'llvm'>
	"Creating a new label is like creating a new basic block (BB)"
	"Future note: we must position the builder at the end of the new BB"
	Transcript show: 'LLVMAppendBasicBlock, ()'; cr. 
]

{ #category : 'API' }
ZagLLVMBuilder >> or: int1 with: int2 [
	
	<primitive: 'or:with:' module: 'llvm'> 
	Transcript show: 'LLVMBuildOr, ('
	<< 'int1: ' << int1 << ' , ' 
	<< 'int2: ' << int2 << ')'; cr.
]

{ #category : 'as yet unclassified' }
ZagLLVMBuilder >> register: oldReg plus: offset asName: newName [

	<primitive: 'register:plus:asName:' module: 'llvm'>
	"Different than add:to: because we want to move the register by 
	the size of the input it currently points to (e.g. a TagObject)"
	Transcript show: 'LLVMBuildGEP2, ('
	<< 'oldReg: ' <<  oldReg << ' , ' 
	<< 'plus: ' << offset << ' , ' 
	<< 'newName: ' << newName << ')'; cr.
]

{ #category : 'API' }
ZagLLVMBuilder >> returnWith: addressOfValue [
	
	<primitive: 'returnWith:' module: 'llvm'> 
	Transcript show: 'LLVMBuildRet, ('
	<< 'addressOfValue: ' << addressOfValue << ')'; cr. 
	
]

{ #category : 'API' }
ZagLLVMBuilder >> shiftLeftArithmeticly: num1 by: num2 [
	
	<primitive: 'shiftLeftArithmeticly:by:' module: 'llvm'> 
	Transcript show: 'LLVMBuildShl, ('
	<< 'num1: ' << num1 << ' , '
	<< 'num2: ' << num2 << ')'; cr. 
	
]

{ #category : 'API' }
ZagLLVMBuilder >> shiftRightArithmeticly: num1 by: num2 [
	
	<primitive: 'shiftLeftArithmeticly:by:' module: 'llvm'> 
	Transcript show: 'LLVMBuildAShr, ('
	<< 'num1: ' << num1 << ' , '
	<< 'num2: ' << num2 << ')'; cr. 
	
]

{ #category : 'API' }
ZagLLVMBuilder >> store: result at: addressOfSelf [ 

	<primitive: 'store:at:' module: 'llvm'>
	Transcript show: 'LLVMBuildStore, ('
	<< 'result: ' << result << ' , '
	<< 'addressOfSelf: ' << addressOfSelf << ')'; cr. 
]

{ #category : 'API' }
ZagLLVMBuilder >> valueAt: aString [ 

	<primitive: 'valueAt:' module: 'llvm'>
	Transcript show: 'LLVMBuildLoad2, ('
	<< 'valueAt: ' << aString << ')'; cr.
	 
]
