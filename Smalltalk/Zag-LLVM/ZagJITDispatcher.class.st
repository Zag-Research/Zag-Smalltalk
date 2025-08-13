"
I have all the logic for generating LLVM JIT code.

I call methods in an LLVM generator object stored in the instance variabe `gen`.

My subclasses simply set `gen` to an appropriate value.
"
Class {
	#name : 'ZagJITDispatcher',
	#superclass : 'Object',
	#traits : 'ZagJITDispatcherTrait',
	#classTraits : 'ZagJITDispatcherTrait classTrait',
	#instVars : [
		'sp',
		'driver',
		'gen'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'API' }
ZagJITDispatcher >> add: aNumber to: selfValue [

	^ gen add: aNumber to: selfValue.
]

{ #category : 'API' }
ZagJITDispatcher >> and: valueRef1 with: valueRef2 [

	^ gen and: valueRef1 with: valueRef2.
]

{ #category : 'converting' }
ZagJITDispatcher >> canEncodeAsSmallInteger: nativeInt [ 
	
	"Check if nativeInt fits in a 56-bit signed SmallInteger payload.
	Shift left 8 bits then arithmetic right 8 bits; if the value is unchanged,
	it means all discarded bits were just sign-extension bits, so the number fits.
	If the value changes, it had significant bits beyond the 56-bit range."
	| temp widened narrowed |
	temp := nativeInt.
	widened := gen shiftRightArithmeticly: temp by: 8.
	narrowed := gen shiftLeftArithmeticly: temp by: 8.
	^ gen compare: narrowed is: #eq to: nativeInt.
]

{ #category : 'accessing' }
ZagJITDispatcher >> driver [

	^ driver
]

{ #category : 'accessing' }
ZagJITDispatcher >> driver: driverSource [ 
	
	driver := driverSource.
]

{ #category : 'private - processing' }
ZagJITDispatcher >> generateLabel [

	^ gen newLabel
]

{ #category : 'accessing' }
ZagJITDispatcher >> generator: aGenerator [

	gen := aGenerator.
]

{ #category : 'testing' }
ZagJITDispatcher >> if: condition isFalseThen: falseLabel [

	| otherewise |
	otherewise :=  self generateLabel.
	self if: condition isFalseThen: falseLabel isTrueThen: otherewise.
	gen defineLabel: otherewise

]

{ #category : 'testing' }
ZagJITDispatcher >> if: condition isFalseThen: falseLabel isTrueThen: trueLabel [ 
	
	gen if: condition isFalseThen: falseLabel isTrueThen: trueLabel.
]

{ #category : 'testing' }
ZagJITDispatcher >> if: condition isTrueThen: trueLabel [

	| otherewise |
	otherewise :=  self generateLabel.
	self if: condition isFalseThen: otherewise isTrueThen: trueLabel.
	gen defineLabel: otherewise

]

{ #category : 'threaded words' }
ZagJITDispatcher >> label [

	| address |
	address := driver thatAddress.
	" generate a label and keep track of the address of this  and the label so when we have a branch or other reference to this address we acan use the label that corresponds to it"
]

{ #category : 'threaded words' }
ZagJITDispatcher >> makeImmediateClosure [

	| value |
	value := driver nextObject

]

{ #category : 'API' }
ZagJITDispatcher >> or: int1 with: int2 [

	^ gen or: int1 with: int2.
]

{ #category : 'primitives' }
ZagJITDispatcher >> primitiveSmallIntegerBinaryOp: aBlock [

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
ZagJITDispatcher >> primitiveSmallIntegerMinus [

	^ self primitiveSmallIntegerBinaryOp: [ :aNumber :selfValue |
		  gen subtract: aNumber from: selfValue ]
]

{ #category : 'primitives' }
ZagJITDispatcher >> primitiveSmallIntegerPlus [

	^ self primitiveSmallIntegerBinaryOp: [ :aNumber :selfValue |
		  gen add: aNumber to: selfValue ]
]

{ #category : 'primitives' }
ZagJITDispatcher >> primitiveSmallIntegerPlusOld [

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
ZagJITDispatcher >> pushBlockNonlocalReturning [

	| value |
	value := driver nextObject.
	" push an immediate block returning the next value. Determined at threaded-code-generation time to be one of the immediate-codable values "
]

{ #category : 'threaded words' }
ZagJITDispatcher >> pushContext [

	driver nextObject. " this is the address of the CompiledMethod that we are compiling for - use it to set up the context "

]

{ #category : 'threaded words' }
ZagJITDispatcher >> pushLiteral [

	| literal name |
	literal := driver nextObject.
	name := #randomName.
	sp := gen register: sp plus: -8 asName: name.
	gen store: literal at: sp
]

{ #category : 'threaded words' }
ZagJITDispatcher >> pushLocal [

	| localNumber |
	localNumber := driver nextObject.
	" push the value of this local in the context "
]

{ #category : 'threaded words' }
ZagJITDispatcher >> returnTop [

	"generate the code to return TOS"
]

{ #category : 'API' }
ZagJITDispatcher >> returnWith: addressOfValue [

	^ self subclassResponsibility
]

{ #category : 'threaded words' }
ZagJITDispatcher >> send [
	" the same as send0 and send1 except it uses the arity of the selector to find the target"

	| selector threadedReturnPoint |
	selector := driver nextObject.
	"need code to extend selector with the class of the target of the send"
	threadedReturnPoint := driver offset: 0
	" we need to save this and the address of our return point in the context"
]

{ #category : 'API' }
ZagJITDispatcher >> shiftLeftArithmeticly: num1 by: num2 [

	^ gen shiftLeftArithmeticly: num1 by: num2.
]

{ #category : 'API' }
ZagJITDispatcher >> shiftRightArithmeticly: num1 by: num2 [

	^ gen shiftRightArithmeticly: num1 by: num2.
]

{ #category : 'converting' }
ZagJITDispatcher >> tagSmallInteger: untaggedInt [

	"Shift left 8 to make room for the tag, then OR with 0x81"
	| shifted |
	shifted := gen shiftLeftArithmeticly: untaggedInt by: 8.
	^ gen or: shifted with: 16r81. 
	
]

{ #category : 'private - processing' }
ZagJITDispatcher >> verifySmallInteger: encodedValue [
	
	"Mask the low byte (0xFF) and compare with SmallInteger's tag (0x81)"
	| andResult |
	andResult := gen and: 16rff with: encodedValue.
	^ gen compare: andResult is: #eq to: 16r81.
]

{ #category : 'instance creation' }
ZagJITDispatcher >> with: aZagOutputLLVMAPI [

	gen := aZagOutputLLVMAPI
]
