"
I set `gen` to the zag runtime object that connects to the LLVM JIT code.
"
Class {
	#name : 'ZagOutputLLVMInImage',
	#superclass : 'ZagOutputLLVM',
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'initialization' }
ZagOutputLLVMInImage >> initPrimitive [

	<primative: 'generator' module: 'llvm'>
	^ self primitiveFailed
]

{ #category : 'initialization' }
ZagOutputLLVMInImage >> initialize [

	super initialize.
	gen := self initPrimitive
]
