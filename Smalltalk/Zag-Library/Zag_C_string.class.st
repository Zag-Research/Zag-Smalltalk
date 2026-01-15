"
I am an immutable `String` that has a null termonation - i.e. the last allocated byte is a 0x0 (nul) byte.

I am used with FFI calls to interact with the C world.

When created from a `String` the result will contain no internal nul bytes because the source will be scanned and the first nul byte will terminate the result, or a nul will be appended.

I report my size as 1 less than the allocated size.
"
Class {
	#name : 'Zag_C_string',
	#superclass : 'String',
	#category : 'Zag-Library-Types',
	#package : 'Zag-Library',
	#tag : 'Types'
}

{ #category : 'converting' }
Zag_C_string >> asByteArray [

	| ba sz |
	sz := self byteSize.
	ba := ByteArray new: sz.
	ba replaceFrom: 1 to: sz with: self startingAt: 1.
	^ba
]

{ #category : 'accessing' }
Zag_C_string >> byteAt: index [
	^(self at: index) asciiValue
]

{ #category : 'accessing' }
Zag_C_string >> byteAt: index put: value [
	self at: index put: value asCharacter.
	^value
]

{ #category : 'accessing' }
Zag_C_string >> byteSize [

	^ super size
]

{ #category : 'accessing' }
Zag_C_string >> size [

	^ super size - 1
]
