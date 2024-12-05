Class {
	#name : 'ZagOutputLLVM',
	#superclass : 'Object',
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'as yet unclassified' }
ZagOutputLLVM >> primitiveSmallIntegerPlus [

	| aNumber gen sp aNumberIsSmallInteger primitiveFail addressOfSelf selfValue nativeANumber nativeSelf nativeResult overflow result |
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
	self primitiveFail. 
]
