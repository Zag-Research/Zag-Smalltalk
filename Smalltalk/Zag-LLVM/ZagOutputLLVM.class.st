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
		'pc',
		'builder'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'private - processing' }
ZagOutputLLVM >> generateLabel [

	^ builder newLabel
]

{ #category : 'accessing' }
ZagOutputLLVM >> generator: aGenerator [

	builder := aGenerator new.
]

{ #category : 'testing' }
ZagOutputLLVM >> if: condition isFalseThen: falseLabel [

	| otherewise |
	otherewise :=  self generateLabel.
	self if: condition isFalseThen: falseLabel isTrueThen: otherewise.
	builder defineLabel: otherewise

]

{ #category : 'testing' }
ZagOutputLLVM >> if: condition isFalseThen: falseLabel isTrueThen: trueLabel [ 
	
	builder if: condition isFalseThen: falseLabel isTrueThen: trueLabel.
]

{ #category : 'testing' }
ZagOutputLLVM >> if: condition isTrueThen: trueLabel [

	| otherewise |
	otherewise :=  self generateLabel.
	self if: condition isFalseThen: otherewise isTrueThen: trueLabel.
	builder defineLabel: otherewise

]

{ #category : 'threaded words' }
ZagOutputLLVM >> label [

	| address |
	address := pc thatAddress.
	" generate a label and keep track of the address of this  and the label so when we have a branch or other reference to this address we acan use the label that corresponds to it"
]

{ #category : 'threaded words' }
ZagOutputLLVM >> makeImmediateClosure [

	| value |
	value := pc next

]

{ #category : 'accessing' }
ZagOutputLLVM >> pc [

	^ pc
]

{ #category : 'accessing' }
ZagOutputLLVM >> pc: anObject [

	pc := anObject
]

{ #category : 'primitives' }
ZagOutputLLVM >> primitiveSmallIntegerBinaryOp: aBlock [

	| aNumber aNumberIsSmallInteger primitiveFail addressOfSelf selfValue nativeANumber nativeSelf nativeResult overflow result |
	"add code to set up builder, main function, entry label"
	primitiveFail := self generateLabel.

	aNumber := builder valueAt: #sp.
	aNumberIsSmallInteger := self verifySmallInteger: aNumber.
	self if: aNumberIsSmallInteger isFalseThen: primitiveFail.

	addressOfSelf := builder register: #sp plus: 8.
	selfValue := builder valueAt: addressOfSelf.
	nativeANumber := builder shiftRightArithmeticly: aNumber by: 8.
	nativeSelf := builder shiftRightArithmeticly: selfValue by: 8.

	nativeResult := aBlock value: nativeANumber value: nativeSelf.
	overflow := self validSmallInteger: nativeResult.
	self if: overflow isFalseThen: primitiveFail.

	result := self tagSmallInteger: nativeResult.
	builder store: result at: addressOfSelf.
	builder returnWith: addressOfSelf.

	" ----- primitiveFail ----- "
	builder defineLabel: primitiveFail.
	"self primitiveFail"
]

{ #category : 'primitives' }
ZagOutputLLVM >> primitiveSmallIntegerMinus [

	^ self primitiveSmallIntegerBinaryOp: [ :aNumber :selfValue |
		  builder subtract: aNumber from: selfValue ]
]

{ #category : 'primitives' }
ZagOutputLLVM >> primitiveSmallIntegerPlus [

	^ self primitiveSmallIntegerBinaryOp: [ :aNumber :selfValue |
		  builder add: aNumber to: selfValue ]
]

{ #category : 'primitives' }
ZagOutputLLVM >> primitiveSmallIntegerPlusOld [

	| aNumber aNumberIsSmallInteger primitiveFail addressOfSelf selfValue nativeANumber nativeSelf nativeResult overflow result |
	aNumber := builder valueAt: #sp.
	aNumberIsSmallInteger := self verifySmallInteger: aNumber.
	primitiveFail := self generateLabel.
	self branchIf: aNumberIsSmallInteger isFalseTo: primitiveFail.
	addressOfSelf := builder register: #sp plus: 8.
	selfValue := builder valueAt: addressOfSelf.
	nativeANumber := builder shiftRightArithmeticly: aNumber by: 8.
	nativeSelf := builder shiftRightArithmeticly: selfValue by: 8.
	nativeResult := builder add: nativeANumber to: nativeSelf.
	overflow := self validSmallInteger: nativeResult.
	self branchIf: overflow isTrueTo: primitiveFail.
	result := self tagSmallInteger: nativeResult.
	builder store: result at: addressOfSelf.
	self returnWithSP: addressOfSelf.
	self defineLabel: primitiveFail.
	self primitiveFail
]

{ #category : 'threaded words' }
ZagOutputLLVM >> pushBlockNonlocalReturning [

	| value |
	value := pc next.
	" push an immediate block returning the next value. Determined at threaded-code-generation time to be one of the immediate-codable values "
]

{ #category : 'threaded words' }
ZagOutputLLVM >> pushContext [

	pc next. " this is the address of the CompiledMethod that we are compiling for - use it to set up the context "

]

{ #category : 'threaded words' }
ZagOutputLLVM >> pushLiteral [

	| literal |
	literal := pc nextObject.
	sp := builder register: sp plus: -8.
	builder store: literal at: sp
]

{ #category : 'threaded words' }
ZagOutputLLVM >> pushLocal [

	| localNumber |
	localNumber := pc next.
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
	selector := pc next.
	"need code to extend selector with the class of the target of the send"
	threadedReturnPoint := pc offset: 0
	" we need to save this and the address of our return point in the context"
]

{ #category : 'accessing' }
ZagOutputLLVM >> source: builderSource [ 
	
	builder := builderSource.
]

{ #category : 'converting' }
ZagOutputLLVM >> tagSmallInteger: untaggedInt [

	^ builder tagSmallInteger: untaggedInt.
	
]

{ #category : 'converting' }
ZagOutputLLVM >> validSmallInteger: nativeInt [ 
	
	| temp res1 res2 |
	temp := nativeInt.
	res1 := builder shiftRightArithmeticly: temp by: 56.
	res2 := builder shiftLeftArithmeticly: temp by: 56.
	^ builder compare: res2 is: #eq to: nativeInt.
]

{ #category : 'private - processing' }
ZagOutputLLVM >> verifySmallInteger: valueRef [
	
	"valueRef is a Zag smallInteger"
	| andResult |
	andResult := builder and: 16rff with: valueRef.
	^ builder compare: andResult is: #eq to: 16r61
]

{ #category : 'instance creation' }
ZagOutputLLVM >> with: aZagOutputLLVMAPI [

	builder := aZagOutputLLVMAPI
]
