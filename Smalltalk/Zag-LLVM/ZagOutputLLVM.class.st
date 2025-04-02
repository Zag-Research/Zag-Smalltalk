"
I have all the logic for generating LLVM JIT code.

I call methods in an LLVM generator object stored in the instance variabe `gen`.

My subclasses simply set `gen` to an appropriate value.
"
Class {
	#name : 'ZagOutputLLVM',
	#superclass : 'Object',
	#traits : 'ZagImageOutputAPI',
	#classTraits : 'ZagImageOutputAPI classTrait',
	#instVars : [
		'sp',
		'gen',
		'driver'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'accessing' }
ZagOutputLLVM >> driver [

	^ driver
]

{ #category : 'accessing' }
ZagOutputLLVM >> driver: anObject [

	driver := anObject
]

{ #category : 'private - processing' }
ZagOutputLLVM >> generateLabel [

	^ gen newLabel
]

{ #category : 'accessing' }
ZagOutputLLVM >> generator: aGenerator [

	gen := aGenerator new.
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

{ #category : 'threaded words' }
ZagOutputLLVM >> label [

	| address |
	address := driver thatAddress.
	" generate a label and keep track of the address of this  and the label so when we have a branch or other reference to this address we acan use the label that corresponds to it"
]

{ #category : 'threaded words' }
ZagOutputLLVM >> makeImmediateClosure [

	| value |
	value := driver nextObject

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
	"self primitiveFail"
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

{ #category : 'threaded words' }
ZagOutputLLVM >> pushBlockNonlocalReturning [

	| value |
	value := driver nextObject.
	" push an immediate block returning the next value. Determined at threaded-code-generation time to be one of the immediate-codable values "
]

{ #category : 'threaded words' }
ZagOutputLLVM >> pushContext [

	driver nextObject. " this is the address of the CompiledMethod that we are compiling for - use it to set up the context "

]

{ #category : 'threaded words' }
ZagOutputLLVM >> pushLiteral [

	| literal |
	literal := driver nextObject.
	sp := gen register: sp plus: -8.
	gen store: literal at: sp
]

{ #category : 'threaded words' }
ZagOutputLLVM >> pushLocal [

	| localNumber |
	localNumber := driver nextObject.
	" push the value of this local in the context "
]

{ #category : 'threaded words' }
ZagOutputLLVM >> returnTop [

	"generate the code to return TOS"
]

{ #category : 'threaded words' }
ZagOutputLLVM >> send [
	" the same as send0 and send1 except it uses the arity of the selector to find the target"

	| selector threadedReturnPoint |
	selector := driver nextObject.
	"need code to extend selector with the class of the target of the send"
	threadedReturnPoint := driver offset: 0
	" we need to save this and the address of our return point in the context"
]

{ #category : 'accessing' }
ZagOutputLLVM >> source: builderSource [ 
	
	gen := builderSource.
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
