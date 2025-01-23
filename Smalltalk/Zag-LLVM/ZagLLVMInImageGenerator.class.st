"
I am a connector to the Zag runtime code.

`new` is a primitive that returns an object that connects to a dispatch table that calls the LLVM-Zig interface to LLVM.
"
Class {
	#name : 'ZagLLVMInImageGenerator',
	#superclass : 'ZagLLVMGeneratorAPI',
	#instVars : [
		'builder'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'instance creation' }
ZagLLVMInImageGenerator class >> new [

	<primitive: 'generatorObject' module: 'llvm'>
]

{ #category : 'API' }
ZagLLVMInImageGenerator >> add: aNumber to: selfValue [

	<primitive: 'add:to:' module: 'llvm'>
]

{ #category : 'API' }
ZagLLVMInImageGenerator >> newLabel [

	<primitive: 'newLabel' module: 'llvm'>
]
