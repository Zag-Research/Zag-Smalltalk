"
I am a connector to the Zag runtime code.

`new` is a primitive that returns an object that connects to a dispatch table that calls the LLVM-Zig interface to LLVM.
"
Class {
	#name : 'ZagOutputLLVMInImage',
	#superclass : 'ZagOutputLLVMAPI',
	#instVars : [
		'builder'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'instance creation' }
ZagOutputLLVMInImage class >> new [

	<primitive: 'generatorObject' module: 'llvm'>
]

{ #category : 'adding' }
ZagOutputLLVMInImage >> add: aNumber to: selfValue [

	<primitive: 'add:to:' module: 'llvm'>
]

{ #category : 'instance creation' }
ZagOutputLLVMInImage >> newLabel [

	<primitive: 'newLabel' module: 'llvm'>
]
