"
I have all the logic for generating LLVM JIT code.

I call methods in an LLVM generator object stored in the instance variabe `gen`.

My subclasses simply set `gen` to an appropriate value.
"
Class {
	#name : 'ZagOutputLLVM',
	#superclass : 'Object',
	#instVars : [
		'gen'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'private - processing' }
ZagOutputLLVM >> generateLabel [

	^ gen newLabel
]

{ #category : 'accessing' }
ZagOutputLLVM >> generator: aGenerator [
	gen := aGenerator
]

{ #category : 'testing' }
ZagOutputLLVM >> if: condition isFalseThen: falseLabel [

	| otherewise |
	otherewise :=  self generateLabel.
	self if: condition isFalseThen: falseLabel isTrueThen: otherewise.
	gen defineLabel: otherewise

]

{ #category : 'testing' }
ZagOutputLLVM >> if: condition isFalseThen: falseLabel isTrueThen: trueLabel [ 
	
	gen if: condition isFalseThen: falseLabel isTrueThen: trueLabel.
]

{ #category : 'testing' }
ZagOutputLLVM >> if: condition isTrueThen: trueLabel [

	| otherewise |
	otherewise :=  self generateLabel.
	self if: condition isFalseThen: otherewise isTrueThen: trueLabel.
	gen defineLabel: otherewise

]

{ #category : 'primitives' }
ZagOutputLLVM >> primitiveSmallIntegerBinaryOp: aBlock [

	| aNumber aNumberIsSmallInteger primitiveFail addressOfSelf selfValue nativeANumber nativeSelf nativeResult overflow result |
	"add code to set up builder, main function, entry label"
	primitiveFail := self generateLabel.

	aNumber := gen valueAt: #sp.
	aNumberIsSmallInteger := self verifySmallInteger: aNumber.
	self if: aNumberIsSmallInteger isFalseThen: primitiveFail.

	addressOfSelf := gen register: #sp plus: 8.
	selfValue := gen valueAt: addressOfSelf.
	nativeANumber := gen shiftRightArithmeticly: aNumber by: 8.
	nativeSelf := gen shiftRightArithmeticly: selfValue by: 8.

	nativeResult := aBlock value: nativeANumber value: nativeSelf.
	overflow := self validSmallInteger: nativeResult.
	self if: overflow isFalseThen: primitiveFail.

	result := self tagSmallInteger: nativeResult.
	gen store: result at: addressOfSelf.
	gen returnWith: addressOfSelf.

	" ----- primitiveFail ----- "
	gen defineLabel: primitiveFail.
	self primitiveFail
]

{ #category : 'primitives' }
ZagOutputLLVM >> primitiveSmallIntegerMinus [

	^ self primitiveSmallIntegerBinaryOp: [ :aNumber :selfValue |
		  gen subtract: aNumber from: selfValue ]
]

{ #category : 'primitives' }
ZagOutputLLVM >> primitiveSmallIntegerPlus [

	^ self primitiveSmallIntegerBinaryOp: [ :aNumber :selfValue |
		  gen add: aNumber to: selfValue ]
]

{ #category : 'primitives' }
ZagOutputLLVM >> primitiveSmallIntegerPlusOld [

	| aNumber aNumberIsSmallInteger primitiveFail addressOfSelf selfValue nativeANumber nativeSelf nativeResult overflow result |
	aNumber := gen valueAt: #sp.
	aNumberIsSmallInteger := self verifySmallInteger: aNumber.
	primitiveFail := self generateLabel.
	self branchIf: aNumberIsSmallInteger isFalseTo: primitiveFail.
	addressOfSelf := gen register: #sp plus: 8.
	selfValue := gen valueAt: addressOfSelf.
	nativeANumber := gen shiftRightArithmeticly: aNumber by: 8.
	nativeSelf := gen shiftRightArithmeticly: selfValue by: 8.
	nativeResult := gen add: nativeANumber to: nativeSelf.
	overflow := self validSmallInteger: nativeResult.
	self branchIf: overflow isTrueTo: primitiveFail.
	result := self tagSmallInteger: nativeResult.
	gen store: result at: addressOfSelf.
	self returnWithSP: addressOfSelf.
	self defineLabel: primitiveFail.
	self primitiveFail
]

{ #category : 'converting' }
ZagOutputLLVM >> tagSmallInteger: untaggedInt [

	^ gen tagSmallInteger: untaggedInt.
	
]

{ #category : 'converting' }
ZagOutputLLVM >> validSmallInteger: nativeInt [ 
	
	| temp res1 res2 |
	temp := nativeInt.
	res1 := gen shiftRightArithmeticly: temp by: 56.
	res2 := gen shiftLeftArithmeticly: temp by: 56.
	^ gen compare: res2 is: #eq to: nativeInt.
]

{ #category : 'private - processing' }
ZagOutputLLVM >> verifySmallInteger: valueRef [
	
	"valueRef is a Zag smallInteger"
	| andResult |
	andResult := gen and: 16rff with: valueRef.
	^ gen compare: andResult is: #eq to: 16r61
]

{ #category : 'instance creation' }
ZagOutputLLVM >> with: aZagOutputLLVMAPI [

	gen := aZagOutputLLVMAPI
]
