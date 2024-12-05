"
An ASZagConstantsTest is a test class for testing the behavior of ASZagConstants
"
Class {
	#name : 'ZagConstantsTest',
	#superclass : 'TestCase',
	#category : 'Zag-Exporter-Tests-Tests',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ZagConstantsTest >> testNanCreation [

	| class |
	class := ZagImageEncoder constantsClass: #ZagConstantsTest
		         from: 'Nan
foo 42
bar 99' readStream.
	self assert: class package name equals: #_UnpackagedPackage.
	self assert: class superclass equals: ZagImageEncodeNan.
	self assert: (class respondsTo: #foo).
	self assert: class foo equals: 42.
	self assert: class bar equals: 99
]

{ #category : 'tests' }
ZagConstantsTest >> testZagCreation [

	| class |
	class := ZagImageEncoder constantsClass: #ZagConstantsTest from: ' " comment " Zag
	classFalse 1
	groupImmediates 1
" with assignment" foo := 42.
bar = 99.
"without" blat 17' readStream.
	self assert: class package name equals: #_UnpackagedPackage.
	self assert: class superclass equals: ZagImageEncodeZag.
	self assert: (class respondsTo: #foo).
	self assert: class foo equals: 42.
	self assert: class bar equals: 99.
	self assert: class blat equals: 17.
	self assert: (class boolean: false) equals: 9.
	self assert: (class boolean: true) equals: 17
]
