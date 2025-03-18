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
]

{ #category : 'API' }
ZagLLVMBuilder >> newLabel [

	<primitive: 'newLabel' module: 'llvm'>
]

{ #category : 'as yet unclassified' }
ZagLLVMBuilder >> register: oldReg plus: offset asName: newName [
	<primitive: 'register:plus:asName:' module: 'llvm'>
]
